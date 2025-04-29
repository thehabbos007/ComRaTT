use crate::types::{TypedExpr, TypedProg, TypedToplevel};
use itertools::Itertools;
use map_box::Map;

use super::Pass;

#[derive(Debug, Default)]
pub struct EliminateConsecLam;

impl Pass for EliminateConsecLam {
    fn run(&mut self, prog: TypedProg) -> TypedProg {
        let defs = prog.defs;
        let defs = defs
            .into_iter()
            .map(|def| match def {
                TypedToplevel::TFunDef(name, args, body, typ) => TypedToplevel::TFunDef(
                    name,
                    args,
                    body.map_box(|expr| self.fuse_lams(expr)),
                    typ,
                ),
                def => def,
            })
            .collect_vec();

        TypedProg {
            defs,
            sorted_inputs: prog.sorted_inputs,
        }
    }
}

impl EliminateConsecLam {
    pub fn new() -> Self {
        Self {}
    }

    fn fuse_lams(&self, expr: TypedExpr) -> TypedExpr {
        match expr {
            TypedExpr::TLam(
                mut args,
                box TypedExpr::TLam(inner_args, inner_body, _, clock2),
                ty,
                clock1,
            ) => {
                args.extend(inner_args);
                let clock = clock1.map(|mut cl| {
                    cl.extend(clock2.unwrap_or_default());
                    cl
                });

                self.fuse_lams(TypedExpr::TLam(args, inner_body, ty, clock))
            }
            TypedExpr::TLam(args, body, ty, clock) => {
                TypedExpr::TLam(args, body.map_box(|v| self.fuse_lams(v)), ty, clock)
            }
            TypedExpr::TApp(typed_expr, arg, ty) => TypedExpr::TApp(
                typed_expr.map_box(|v| self.fuse_lams(v)),
                arg.into_iter().map(|v| self.fuse_lams(v)).collect_vec(),
                ty,
            ),
            TypedExpr::TPrim(binop, left, right, ty) => TypedExpr::TPrim(
                binop,
                left.map_box(|expr| self.fuse_lams(expr)),
                right.map_box(|expr| self.fuse_lams(expr)),
                ty,
            ),
            TypedExpr::TLet(name, ty, rhs, body) => TypedExpr::TLet(
                name,
                ty,
                rhs.map_box(|expr| self.fuse_lams(expr)),
                body.map_box(|expr| self.fuse_lams(expr)),
            ),
            TypedExpr::TIfThenElse(cond, then_branch, else_branch, ty) => TypedExpr::TIfThenElse(
                cond.map_box(|v| self.fuse_lams(v)),
                then_branch.map_box(|v| self.fuse_lams(v)),
                else_branch.map_box(|v| self.fuse_lams(v)),
                ty,
            ),
            TypedExpr::TTuple(vec, ty) => {
                TypedExpr::TTuple(vec.into_iter().map(|v| self.fuse_lams(v)).collect_vec(), ty)
            }
            TypedExpr::TAccess(arg, access, ty) => {
                TypedExpr::TAccess(arg.map_box(|v| self.fuse_lams(v)), access, ty)
            }
            TypedExpr::TName(_, _) | TypedExpr::TConst(_, _) | TypedExpr::TWait(_, _) => expr,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;
    // Helper functions for creating typed expressions in tests
    use crate::source::{Binop, Type};

    // Helper to construct a type with multiple arguments
    fn construct_type(types: &[&str]) -> Type {
        if types.is_empty() {
            panic!("Cannot construct a type from empty list");
        }

        let mut result = Type::TInt; // Default to int for simplicity

        for &t in types.iter().rev().skip(1) {
            match t {
                "Int" => {
                    result = Type::TFun(Box::new(Type::TInt), Box::new(result));
                }
                _ => panic!("Unsupported type in test: {}", t),
            }
        }

        result
    }

    // Helper to create a lambda expression
    fn make_lam(args: Vec<(String, Type)>, body: TypedExpr, typ: Type) -> TypedExpr {
        TypedExpr::TLam(args, Box::new(body), typ, None)
    }

    // Helper to create a name expression
    fn make_name(name: &str, typ: Type) -> TypedExpr {
        TypedExpr::TName(name.to_string(), typ)
    }

    #[test]
    fn test_multi_arg_lams() {
        // Input:    fun x y -> fun w h -> x + y + w + h
        // Expected: fun x y w h -> x + y + w + h

        let x_name = make_name("x", Type::TInt);
        let y_name = make_name("y", Type::TInt);
        let w_name = make_name("w", Type::TInt);
        let h_name = make_name("h", Type::TInt);

        let x_plus_y = TypedExpr::TPrim(Binop::Add, Box::new(x_name), Box::new(y_name), Type::TInt);

        let xy_plus_w =
            TypedExpr::TPrim(Binop::Add, Box::new(x_plus_y), Box::new(w_name), Type::TInt);

        let xyw_plus_h = TypedExpr::TPrim(
            Binop::Add,
            Box::new(xy_plus_w),
            Box::new(h_name),
            Type::TInt,
        );

        let inner_lam = make_lam(
            vec![("w".to_string(), Type::TInt), ("h".to_string(), Type::TInt)],
            xyw_plus_h,
            Type::TFun(
                Box::new(Type::TInt),
                Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
            ),
        );

        let outer_lam = make_lam(
            vec![("x".to_string(), Type::TInt), ("y".to_string(), Type::TInt)],
            inner_lam,
            Type::TFun(
                Box::new(Type::TInt),
                Box::new(Type::TFun(
                    Box::new(Type::TInt),
                    Box::new(Type::TFun(
                        Box::new(Type::TInt),
                        Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
                    )),
                )),
            ),
        );

        let pass = EliminateConsecLam::new();
        let result = pass.fuse_lams(outer_lam);

        assert_matches!(
            result,
            TypedExpr::TLam(
                args,
                box TypedExpr::TPrim(Binop::Add,
                    box TypedExpr::TPrim(Binop::Add, box TypedExpr::TPrim(Binop::Add, _, _, Type::TInt), _, Type::TInt),
                    _,
                    Type::TInt),
                _,
                _
            ) if args.as_slice() == [
                ("x".into(), Type::TInt),
                ("y".into(), Type::TInt),
                ("w".into(), Type::TInt),
                ("h".into(), Type::TInt)
            ],
        );
    }

    #[test]
    fn test_fuse_lams() {
        let pass = EliminateConsecLam::new();

        // Create a nested lambda: fun x -> fun y -> x
        let x_name = make_name("x", Type::TInt);
        let inner_lam = make_lam(
            vec![("y".to_string(), Type::TInt)],
            x_name.clone(),
            construct_type(&["Int", "Int", "Int"]),
        );

        let outer_lam = make_lam(
            vec![("x".to_string(), Type::TInt)],
            inner_lam,
            construct_type(&["Int", "Int", "Int", "Int"]),
        );

        // Apply the pass
        let result = pass.fuse_lams(outer_lam);

        // Expected: fun x y -> x
        match result {
            TypedExpr::TLam(args, body, ..) => {
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].0, "x");
                assert_eq!(args[1].0, "y");

                // Check that the body is just "x"
                match *body {
                    TypedExpr::TName(name, _) => {
                        assert_eq!(name, "x");
                    }
                    _ => panic!("Expected TName in lambda body"),
                }
            }
            _ => panic!("Expected TLam after fusion"),
        }
    }

    #[test]
    fn test_nested_lams() {
        let pass = EliminateConsecLam::new();

        // Create a triply-nested lambda: fun x -> fun y -> fun z -> x
        let x_name = make_name("x", Type::TInt);

        let inner_lam = make_lam(
            vec![("z".to_string(), Type::TInt)],
            x_name.clone(),
            construct_type(&["Int", "Int"]),
        );

        let middle_lam = make_lam(
            vec![("y".to_string(), Type::TInt)],
            inner_lam,
            construct_type(&["Int", "Int", "Int"]),
        );

        let outer_lam = make_lam(
            vec![("x".to_string(), Type::TInt)],
            middle_lam,
            construct_type(&["Int", "Int", "Int", "Int"]),
        );

        // Apply the pass
        let result = pass.fuse_lams(outer_lam);

        // Expected: fun x y z -> x
        match result {
            TypedExpr::TLam(args, body, ..) => {
                assert_eq!(args.len(), 3);
                assert_eq!(args[0].0, "x");
                assert_eq!(args[1].0, "y");
                assert_eq!(args[2].0, "z");

                // Check that the body is just "x"
                match *body {
                    TypedExpr::TName(name, _) => {
                        assert_eq!(name, "x");
                    }
                    _ => panic!("Expected TName in lambda body"),
                }
            }
            _ => panic!("Expected TLam after fusion"),
        }
    }

    #[test]
    fn test_run() {
        let mut pass = EliminateConsecLam::new();

        // Create a function definition with nested lambdas
        let x_name = make_name("x", Type::TInt);
        let inner_lam = make_lam(
            vec![("y".to_string(), Type::TInt)],
            x_name.clone(),
            construct_type(&["Int", "Int"]),
        );

        let outer_lam = make_lam(
            vec![("x".to_string(), Type::TInt)],
            inner_lam,
            construct_type(&["Int", "Int", "Int"]),
        );

        let fun_def = TypedToplevel::TFunDef(
            "test_fun".to_string(),
            vec![],
            Box::new(outer_lam),
            construct_type(&["Int", "Int", "Int"]),
        );

        let prog = TypedProg(vec![fun_def], vec![]);
        let result = pass.run(prog);

        // Check the result
        match &result.defs[0] {
            TypedToplevel::TFunDef(name, _, body, _) => {
                assert_eq!(name, "test_fun");

                // Check that the body is a fused lambda
                match &**body {
                    TypedExpr::TLam(args, ..) => {
                        assert_eq!(args.len(), 2);
                        assert_eq!(args[0].0, "x");
                        assert_eq!(args[1].0, "y");
                    }
                    _ => panic!("Expected TLam in function body"),
                }
            }
            _ => panic!("Expected TFunDef in result"),
        }
    }
}
