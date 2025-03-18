use crate::source::Type;
use crate::types::{
    build_function_type, find_free_vars, tfun_len_n, TypedExpr, TypedProg, TypedToplevel,
};
use itertools::Itertools;
use std::collections::HashSet;

use super::Pass;

#[derive(Debug)]
pub struct LambdaLift {
    counter: usize,
}

impl Pass for LambdaLift {
    fn run(&mut self, prog: TypedProg) -> TypedProg {
        let defs = prog.0;

        let (lifted_defs, lifted_lambdas): (Vec<_>, Vec<_>) = defs
            .into_iter()
            .map(|def| match def {
                TypedToplevel::TFunDef(name, args, body, typ) => {
                    let (lifted_body, lambda_defs) = self.lambda_lift(&args, *body);
                    (
                        TypedToplevel::TFunDef(name, args, lifted_body.b(), typ),
                        lambda_defs,
                    )
                }
                def => (def, vec![]),
            })
            .unzip();

        let lifted_lambdas = lifted_lambdas.into_iter().flatten().collect_vec();

        lifted_defs
            .into_iter()
            .chain(lifted_lambdas)
            .collect_vec()
            .into()
    }
}

impl Default for LambdaLift {
    fn default() -> Self {
        Self::new()
    }
}

impl LambdaLift {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    fn unique_name(&mut self, prefix: &str) -> String {
        self.counter += 1;
        format!("#{prefix}_{}", self.counter)
    }

    // Find all free variables in an expression

    fn lift_lambdas(
        &mut self,
        expr: TypedExpr,
        ctx: &[(String, Type)],
    ) -> (TypedExpr, Vec<TypedToplevel>) {
        match expr {
            TypedExpr::TConst(_, _) => (expr, vec![]),
            TypedExpr::TName(_, _) => (expr, vec![]),

            TypedExpr::TPrim(op, left, right, typ) => {
                let (left_expr, mut left_defs) = self.lift_lambdas(*left, ctx);
                let (right_expr, right_defs) = self.lift_lambdas(*right, ctx);
                left_defs.extend(right_defs);
                (
                    TypedExpr::TPrim(op, Box::new(left_expr), Box::new(right_expr), typ),
                    left_defs,
                )
            }

            TypedExpr::TLam(args, body, typ) => {
                let bound: HashSet<_> = args.iter().map(|(name, _)| name.clone()).collect();
                let free_vars = find_free_vars(&body, &bound);
                let free_vars_with_types = free_vars
                    .iter()
                    .filter_map(|name| {
                        ctx.iter()
                            .find(|(n, _)| n == name)
                            .map(|(name, typ)| (name.clone(), typ.clone()))
                    })
                    .collect_vec();

                if free_vars_with_types.is_empty() {
                    let (lifted_body, lifted_defs) = self.lift_lambdas(*body, ctx);
                    let lambda = TypedExpr::TLam(args, Box::new(lifted_body), typ);
                    (lambda, lifted_defs)
                } else {
                    let fun_name = self.unique_name("lambda");
                    let orig_arg_len = args.len();
                    let mut new_args = free_vars_with_types.clone();
                    new_args.extend(args);

                    let (return_typ, _) = tfun_len_n(typ.clone(), orig_arg_len);

                    let (lifted_body, mut lifted_defs) = self.lift_lambdas(*body, &new_args);

                    let lambda_def = TypedToplevel::TFunDef(
                        fun_name.clone(),
                        new_args.clone(),
                        Box::new(lifted_body),
                        return_typ.clone(),
                    );

                    let substitute_typ = build_function_type(
                        &new_args.clone().into_iter().map(|(_, ty)| ty).collect_vec(),
                        return_typ,
                    );

                    lifted_defs.push(lambda_def);

                    let app_args: Vec<_> = free_vars_with_types
                        .iter()
                        .map(|(name, typ)| TypedExpr::TName(name.clone(), typ.clone()))
                        .collect();

                    (
                        TypedExpr::TApp(
                            Box::new(TypedExpr::TName(fun_name, substitute_typ.clone())),
                            app_args,
                            typ,
                        ),
                        lifted_defs,
                    )
                }
            }

            TypedExpr::TApp(fn_expr, args, typ) => {
                let (lifted_fn, mut fn_defs) = self.lift_lambdas(*fn_expr, ctx);
                let mut all_defs = vec![];
                let mut lifted_args = vec![];

                for arg in args {
                    let (lifted_arg, arg_defs) = self.lift_lambdas(arg, ctx);
                    lifted_args.push(lifted_arg);
                    all_defs.extend(arg_defs);
                }

                fn_defs.extend(all_defs);
                (
                    TypedExpr::TApp(Box::new(lifted_fn), lifted_args, typ),
                    fn_defs,
                )
            }

            TypedExpr::TLet(name, typ, rhs, body) => {
                let (lifted_rhs, mut rhs_defs) = self.lift_lambdas(*rhs, ctx);
                let mut new_ctx = ctx.to_vec();
                new_ctx.push((name.clone(), typ.clone()));
                let (lifted_body, body_defs) = self.lift_lambdas(*body, &new_ctx);
                rhs_defs.extend(body_defs);
                (
                    TypedExpr::TLet(name, typ, Box::new(lifted_rhs), Box::new(lifted_body)),
                    rhs_defs,
                )
            }

            TypedExpr::TIfThenElse(condition, then_branch, else_branch, typ) => {
                let (lifted_cond, mut cond_defs) = self.lift_lambdas(*condition, ctx);
                let (lifted_then, then_defs) = self.lift_lambdas(*then_branch, ctx);
                let (lifted_else, else_defs) = self.lift_lambdas(*else_branch, ctx);

                cond_defs.extend(then_defs);
                cond_defs.extend(else_defs);

                (
                    TypedExpr::TIfThenElse(
                        Box::new(lifted_cond),
                        Box::new(lifted_then),
                        Box::new(lifted_else),
                        typ,
                    ),
                    cond_defs,
                )
            }

            TypedExpr::TTuple(exprs, typ) => {
                let mut all_defs = vec![];
                let mut lifted_exprs = vec![];

                for expr in exprs {
                    let (lifted_expr, defs) = self.lift_lambdas(expr, ctx);
                    lifted_exprs.push(lifted_expr);
                    all_defs.extend(defs);
                }

                (TypedExpr::TTuple(lifted_exprs, typ), all_defs)
            }

            TypedExpr::TAccess(expr, idx, typ) => {
                let (lifted_expr, defs) = self.lift_lambdas(*expr, ctx);
                (TypedExpr::TAccess(Box::new(lifted_expr), idx, typ), defs)
            }
        }
    }

    pub fn lambda_lift(
        &mut self,
        defs: &[(String, Type)],
        expr: TypedExpr,
    ) -> (TypedExpr, Vec<TypedToplevel>) {
        self.lift_lambdas(expr, defs)
    }
}

#[cfg(test)]
#[allow(unused)]
mod tests {
    use super::*;
    use crate::source::{Binop, Const};

    fn int(n: i32) -> TypedExpr {
        TypedExpr::TConst(Const::CInt(n), Type::TInt)
    }

    fn bool(b: bool) -> TypedExpr {
        TypedExpr::TConst(Const::CBool(b), Type::TBool)
    }

    fn var(name: &str, typ: Type) -> TypedExpr {
        TypedExpr::TName(name.to_string(), typ)
    }

    fn add(left: TypedExpr, right: TypedExpr) -> TypedExpr {
        TypedExpr::TPrim(Binop::Add, Box::new(left), Box::new(right), Type::TInt)
    }

    fn eq(left: TypedExpr, right: TypedExpr) -> TypedExpr {
        TypedExpr::TPrim(Binop::Eq, Box::new(left), Box::new(right), Type::TBool)
    }

    fn lam(arg_name: &str, arg_type: Type, body: TypedExpr, ret_type: Type) -> TypedExpr {
        TypedExpr::TLam(
            vec![(arg_name.to_string(), arg_type.clone())],
            Box::new(body),
            Type::TFun(arg_type.b(), Box::new(ret_type)),
        )
    }

    fn let_expr(name: &str, typ: Type, rhs: TypedExpr, body: TypedExpr) -> TypedExpr {
        TypedExpr::TLet(name.to_string(), typ, Box::new(rhs), Box::new(body))
    }

    fn app(func: TypedExpr, arg: TypedExpr, ret_type: Type) -> TypedExpr {
        TypedExpr::TApp(Box::new(func), vec![arg], ret_type)
    }

    fn if_then_else(
        cond: TypedExpr,
        then_branch: TypedExpr,
        else_branch: TypedExpr,
        typ: Type,
    ) -> TypedExpr {
        TypedExpr::TIfThenElse(
            Box::new(cond),
            Box::new(then_branch),
            Box::new(else_branch),
            typ,
        )
    }

    // Helper for verifying lifted functions
    fn assert_lifted_function(
        globals: &[TypedToplevel],
        expected_args_count: usize,
        expected_free_vars: &[&str],
    ) {
        assert!(!globals.is_empty(), "Expected lifted functions");

        let TypedToplevel::TFunDef(name, args, _, _) = &globals[0] else {
            panic!("Didn't get fun def")
        };

        assert!(name.starts_with("#lambda"), "Expected lifted lambda name");
        assert_eq!(
            args.len(),
            expected_args_count + expected_free_vars.len(),
            "Incorrect number of arguments in lifted function"
        );
        for free_var in expected_free_vars {
            assert!(
                args.iter().any(|(name, _)| name == free_var),
                "Missing free variable in lifted function arguments"
            );
        }
    }

    #[test]
    fn test_lambda_lifting_simple() {
        let mut lifter = LambdaLift::new();

        let expr = let_expr(
            "x",
            Type::TInt,
            int(1),
            lam(
                "y",
                Type::TInt,
                add(var("x", Type::TInt), var("y", Type::TInt)),
                Type::TInt,
            ),
        );

        let (lifted, globals) = lifter.lambda_lift(&[], expr);

        assert_lifted_function(&globals, 1, &["x"]);

        // Verify the structure of the lifted expression
        if let TypedExpr::TLet(name, typ, rhs, body) = lifted {
            assert_eq!(name, "x");
            assert_eq!(typ, Type::TInt);
            assert!(matches!(
                *rhs,
                TypedExpr::TConst(Const::CInt(1), Type::TInt)
            ));
            assert!(matches!(*body, TypedExpr::TApp(_, _, _)));
        } else {
            panic!("Expected TLet expression");
        }
    }

    #[test]
    fn test_lambda_lifting_nested() {
        let mut lifter = LambdaLift::new();

        let expr = let_expr(
            "x",
            Type::TInt,
            int(1),
            let_expr(
                "y",
                Type::TInt,
                int(2),
                lam(
                    "z",
                    Type::TInt,
                    add(
                        add(var("x", Type::TInt), var("y", Type::TInt)),
                        var("z", Type::TInt),
                    ),
                    Type::TInt,
                ),
            ),
        );

        let (_lifted, globals) = lifter.lambda_lift(&[], expr);

        assert_lifted_function(&globals, 1, &["x", "y"]);
    }

    #[test]
    fn test_lambda_lifting_with_if() {
        let mut lifter = LambdaLift::new();

        let expr = let_expr(
            "x",
            Type::TInt,
            int(1),
            lam(
                "y",
                Type::TInt,
                if_then_else(
                    eq(var("x", Type::TInt), var("y", Type::TInt)),
                    int(1),
                    int(0),
                    Type::TInt,
                ),
                Type::TInt,
            ),
        );

        let (_lifted, globals) = lifter.lambda_lift(&[], expr);

        assert_lifted_function(&globals, 1, &["x"]);
    }

    #[test]
    fn test_lambda_lifting_no_free_vars() {
        let mut lifter = LambdaLift::new();

        let expr = lam(
            "x",
            Type::TInt,
            add(var("x", Type::TInt), int(1)),
            Type::TInt,
        );

        let (lifted, globals) = lifter.lambda_lift(&[], expr);

        assert!(globals.is_empty(), "Should not have lifted any functions");
        assert!(matches!(lifted, TypedExpr::TLam(_, _, _)));
    }

    #[test]
    fn test_lambda_lifting_nested_lambdas() {
        let mut lifter = LambdaLift::new();

        let expr = let_expr(
            "x",
            Type::TInt,
            int(1),
            lam(
                "y",
                Type::TInt,
                lam(
                    "z",
                    Type::TInt,
                    add(
                        add(var("x", Type::TInt), var("y", Type::TInt)),
                        var("z", Type::TInt),
                    ),
                    Type::TInt,
                ),
                Type::TInt,
            ),
        );

        let (_lifted, globals) = lifter.lambda_lift(&[], expr);

        assert_eq!(globals.len(), 2, "Should have lifted two functions");
        // The inner lambda should be lifted first
        assert_lifted_function(&globals[0..1], 1, &["x", "y"]);
        // The outer lambda should be lifted second
        assert_lifted_function(&globals[1..], 1, &["x"]);
    }

    #[test]
    fn test_lambda_lifting_with_application() {
        // Before:
        // let x = 1 in
        // let f = (fun y -> x + y) in
        // f 2
        //
        // After:
        // let x = 1 in
        // let f = #lambda_1 x in
        // f 2
        //
        // Where #lambda_1 is:
        // def #lambda_1 x y = x + y
        let mut lifter = LambdaLift::new();

        let expr = let_expr(
            "x",
            Type::TInt,
            int(1),
            let_expr(
                "f",
                Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt)),
                lam(
                    "y",
                    Type::TInt,
                    add(var("x", Type::TInt), var("y", Type::TInt)),
                    Type::TInt,
                ),
                app(
                    var("f", Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
                    int(2),
                    Type::TInt,
                ),
            ),
        );

        let (lifted, globals) = lifter.lambda_lift(&[], expr);

        assert_lifted_function(&globals, 1, &["x"]);

        // Verify the application is preserved in the lifted expression
        if let TypedExpr::TLet(name, _, _, body) = lifted
            && name == "x"
        {
            if let TypedExpr::TLet(name, _, rhs, inner_body) = *body
                && name == "f"
            {
                assert!(
                    matches!(*rhs, TypedExpr::TApp(box TypedExpr::TName(name, _), _, _) if name == "#lambda_1")
                );

                assert!(
                    matches!(*inner_body, TypedExpr::TApp(box TypedExpr::TName(name, _), _, _) if name == "f")
                );
            } else {
                panic!("Expected nested let expression");
            }
        } else {
            panic!("Expected TLet expression");
        }
    }
    #[test]
    fn test_lambda_lifting_tuple() {
        let mut lifter = LambdaLift::new();

        let expr = let_expr(
            "x",
            Type::TInt,
            int(1),
            lam(
                "y",
                Type::TInt,
                TypedExpr::TTuple(
                    vec![var("x", Type::TInt), var("y", Type::TInt)],
                    Type::TProduct(vec![Type::TInt, Type::TInt]),
                ),
                Type::TProduct(vec![Type::TInt, Type::TInt]),
            ),
        );

        let (_lifted, globals) = lifter.lambda_lift(&[], expr);

        assert_lifted_function(&globals, 1, &["x"]);
    }

    #[test]
    fn test_lambda_lifting_tuple_access() {
        let mut lifter = LambdaLift::new();

        let expr = let_expr(
            "x",
            Type::TInt,
            int(1),
            lam(
                "t",
                Type::TProduct(vec![Type::TInt, Type::TInt]),
                add(
                    var("x", Type::TInt),
                    TypedExpr::TAccess(
                        Box::new(var("t", Type::TProduct(vec![Type::TInt, Type::TInt]))),
                        0,
                        Type::TInt,
                    ),
                ),
                Type::TInt,
            ),
        );

        let (_lifted, globals) = lifter.lambda_lift(&[], expr);

        assert_lifted_function(&globals, 1, &["x"]);
    }
}
