use std::collections::HashMap;

use crate::source::Type;
use crate::types::{
    build_function_type, final_type, unpack_type, BindingContext, BindingKind, TypedExpr,
    TypedProg, TypedToplevel,
};
use itertools::Itertools;
use map_box::Map as _;

use super::Pass;

#[derive(Debug, Default)]
pub struct PartialElimination {
    handle_delayed_closures: bool,
    var_counter: usize,
    toplevels: BindingContext,
}

impl Pass for PartialElimination {
    fn run(&mut self, prog: TypedProg) -> TypedProg {
        self.toplevels = prog
            .defs
            .iter()
            .filter_map(|def| match def {
                TypedToplevel::TFunDef(name, args, _, ret_ty) => {
                    let ty = build_function_type(
                        args.iter()
                            .cloned()
                            .map(|(_, ty)| ty)
                            .collect_vec()
                            .as_slice(),
                        ret_ty.clone(),
                    );

                    Some(((name.clone(), ty), BindingKind::Toplevel))
                }
                _ => None,
            })
            .collect::<HashMap<_, _>>();

        let defs = prog.defs;
        let defs = defs
            .into_iter()
            .map(|def| match def {
                TypedToplevel::TFunDef(name, args, body, typ) => TypedToplevel::TFunDef(
                    name,
                    args,
                    body.map_box(|b| self.eliminate_partial(b)),
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

impl PartialElimination {
    pub fn new(handle_delayed_closures: bool) -> Self {
        Self {
            handle_delayed_closures,
            toplevels: HashMap::new(),
            var_counter: 0,
        }
    }

    pub fn set_handle_delayed_closures(&mut self, toggle: bool) {
        self.handle_delayed_closures = toggle
    }

    fn unique_var_name(&mut self, x: &str) -> String {
        self.var_counter += 1;
        format!("#{x}_{}", self.var_counter)
    }

    fn generate_lambda_vars_and_app_vars(
        &mut self,
        eta_args: &[Type],
    ) -> (Vec<(String, Type)>, Vec<TypedExpr>) {
        match eta_args {
            [] => (vec![], vec![]),
            [typ, rest @ ..] => {
                let name = self.unique_var_name("part_elim_lam");
                let var = TypedExpr::TName(name.clone(), typ.clone());
                let (mut lambda, mut app) = self.generate_lambda_vars_and_app_vars(rest);
                lambda.insert(0, (name, typ.clone()));
                app.insert(0, var);
                (lambda, app)
            }
        }
    }

    fn handle_type_is_later_closure(&self, typ: &Type) -> bool {
        self.handle_delayed_closures || !typ.contains_later_unit()
    }

    fn handle_later_closure(&self, name: &str, typ: &Type) -> bool {
        self.handle_type_is_later_closure(typ)
            && !self.toplevels.contains_key(&(name.to_owned(), typ.clone()))
    }

    pub fn eliminate_partial(&mut self, texpr: TypedExpr) -> TypedExpr {
        match texpr {
            TypedExpr::TName(name, ty @ Type::TFun(..))
                // if self.handle_later_closure(&name, &ty)
                =>
            {
                let eta_args = unpack_type(&ty);
                let (lambda_args, app_args) = self.generate_lambda_vars_and_app_vars(&eta_args);
                TypedExpr::TLam(
                    lambda_args,
                    Box::new(TypedExpr::TApp(
                        TypedExpr::TName(name.to_owned(), ty.clone()).b(),
                        app_args,
                        final_type(&ty),
                    )),
                    ty.clone(),
                    None
                )
            }
            TypedExpr::TConst(_, _) | TypedExpr::TName(_, _) | TypedExpr::TWait(_, _) => texpr,
            TypedExpr::TPrim(op, left, right, typ) => TypedExpr::TPrim(
                op,
                Box::new(self.eliminate_partial(*left)),
                Box::new(self.eliminate_partial(*right)),
                typ,
            ),
            TypedExpr::TLam(args, body, typ, clock) => {
                TypedExpr::TLam(args, Box::new(self.eliminate_partial(*body)), typ, clock)
            }
            TypedExpr::TApp(fun_expr, args, app_ty @ Type::TFun(_, _)) =>
            {
                let eta_args = unpack_type(&app_ty);
                let (lambda_args, app_args) = self.generate_lambda_vars_and_app_vars(&eta_args);
                let mut all_args = args;
                all_args.extend(app_args);
                TypedExpr::TLam(
                    lambda_args,
                    Box::new(TypedExpr::TApp(fun_expr, all_args, final_type(&app_ty))),
                    app_ty.clone(),
                    None
                )
            }
            // TODO: Should we just do this in anf conversion instead? It's so special cased.
            // TypedExpr::TApp(fun_expr, args, Type::TLater(ty, clock)) if self.handle_delayed_closures => {
            //     TypedExpr::TLam(
            //         Vec::new(),
            //         fun_expr,
            //         Type::TLater(ty, clock.clone()),
            //         Some(clock)
            //     )
            // }
            TypedExpr::TApp(fn_expr, args, typ) => TypedExpr::TApp(
                fn_expr, args,
                // Don't think we want to eliminate partials in the args of a TApp because
                // that means higher order functions in some cases.
                // args.into_iter()
                //     .map(|arg| self.eliminate_partial(arg))
                //     .collect(),
                typ,
            ),
            TypedExpr::TLet(name, typ, rhs, body) => TypedExpr::TLet(
                name,
                typ,
                Box::new(self.eliminate_partial(*rhs)),
                Box::new(self.eliminate_partial(*body)),
            ),
            TypedExpr::TIfThenElse(condition, then_branch, else_branch, typ) => {
                TypedExpr::TIfThenElse(
                    Box::new(self.eliminate_partial(*condition)),
                    Box::new(self.eliminate_partial(*then_branch)),
                    Box::new(self.eliminate_partial(*else_branch)),
                    typ,
                )
            }
            TypedExpr::TTuple(texps, typ) => TypedExpr::TTuple(
                texps
                    .into_iter()
                    .map(|texp| self.eliminate_partial(texp))
                    .collect(),
                typ,
            ),
            TypedExpr::TAccess(texp, idx, typ) => {
                TypedExpr::TAccess(Box::new(self.eliminate_partial(*texp)), idx, typ)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        source::{Binop, Const, Type},
        types::TypedExpr,
    };

    #[test]
    fn test_final_type() {
        // Input: int -> bool -> int
        // Output: int
        let ty = Type::TFun(
            Box::new(Type::TInt),
            Box::new(Type::TFun(Box::new(Type::TBool), Box::new(Type::TInt))),
        );
        assert_eq!(final_type(&ty), Type::TInt);
    }

    #[test]
    fn test_final_type_product() {
        // Input: (int, bool, unit)
        // Output: unit
        let ty = Type::TProduct(vec![Type::TInt, Type::TBool, Type::TUnit]);
        assert_eq!(final_type(&ty), Type::TUnit);
    }

    #[test]
    #[should_panic(
        expected = "final_type_tproduct: Attempted to take final type of empty tproduct"
    )]
    fn test_final_type_product_empty() {
        // Input: ()
        // Output: panic
        let ty = Type::TProduct(vec![]);
        final_type(&ty);
    }

    #[test]
    fn test_eliminate_partial_simple_let() {
        // Before: let x = y in x
        // After:  let x = y in x
        let mut eliminator = PartialElimination::new(false);
        let expr = TypedExpr::TLet(
            "x".to_string(),
            Type::TInt,
            Box::new(TypedExpr::TName("y".to_string(), Type::TInt)),
            Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
        );
        let result = eliminator.eliminate_partial(expr.clone());
        assert_eq!(result, expr);
    }

    #[test]
    fn test_eliminate_partial_nested_let_with_partial() {
        // Before: let f = (fun x y -> x + y) in let g = f in g 42
        // After:  let f = (fun x y -> x + y) in (fun y -> f 42 y)
        let mut eliminator = PartialElimination::new(false);
        let expr = TypedExpr::TLet(
            "f".to_string(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
            Box::new(TypedExpr::TLam(
                vec![("x".to_string(), Type::TInt), ("y".to_string(), Type::TInt)],
                Box::new(TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                    Box::new(TypedExpr::TName("y".to_string(), Type::TInt)),
                    Type::TInt,
                )),
                Type::TFun(
                    Type::TInt.b(),
                    Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                ),
                None,
            )),
            Box::new(TypedExpr::TLet(
                "g".to_string(),
                Type::TFun(
                    Type::TInt.b(),
                    Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                ),
                Box::new(TypedExpr::TName(
                    "f".to_string(),
                    Type::TFun(
                        Type::TInt.b(),
                        Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                    ),
                )),
                Box::new(TypedExpr::TApp(
                    Box::new(TypedExpr::TName(
                        "g".to_string(),
                        Type::TFun(
                            Type::TInt.b(),
                            Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                        ),
                    )),
                    vec![TypedExpr::TConst(Const::CInt(42), Type::TInt)],
                    Type::TFun(Type::TInt.b(), Type::TInt.b()),
                )),
            )),
        );

        let expected_rhs = TypedExpr::TLam(
            vec![("x".to_owned(), Type::TInt), ("y".to_owned(), Type::TInt)],
            TypedExpr::TPrim(
                Binop::Add,
                TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                TypedExpr::TName("y".to_owned(), Type::TInt).b(),
                Type::TInt,
            )
            .b(),
            Type::TFun(
                Type::TInt.b(),
                Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
            ),
            None,
        );

        let expected_body = TypedExpr::TLam(
            vec![("#part_elim_lam_1".to_owned(), Type::TInt)],
            TypedExpr::TApp(
                Box::new(TypedExpr::TName(
                    "f".to_owned(),
                    Type::TFun(
                        Type::TInt.b(),
                        Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                    ),
                )),
                vec![
                    TypedExpr::TConst(Const::CInt(42), Type::TInt),
                    TypedExpr::TName("#part_elim_lam_1".to_owned(), Type::TInt),
                ],
                Type::TInt,
            )
            .b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
            None,
        );

        let result = eliminator.eliminate_partial(expr);
        if let TypedExpr::TLet(name, typ, box rhs, box body) = result {
            assert_eq!(name, "f".to_owned());
            assert_eq!(typ, Type::TFun(Type::TInt.b(), Type::TInt.b()));
            assert_eq!(rhs, expected_rhs);
            assert_eq!(body, expected_body);
        };
    }

    #[test]
    fn test_eliminate_partial_nested_let() {
        // Before: let x = y in let z = x in z
        // After:  y
        let mut eliminator = PartialElimination::new(false);
        let expr = TypedExpr::TLet(
            "x".to_string(),
            Type::TInt,
            Box::new(TypedExpr::TName("y".to_string(), Type::TInt)),
            Box::new(TypedExpr::TLet(
                "z".to_string(),
                Type::TInt,
                Box::new(TypedExpr::TName("x".to_string(), Type::TInt)),
                Box::new(TypedExpr::TName("z".to_string(), Type::TInt)),
            )),
        );
        let result = eliminator.eliminate_partial(expr);
        assert_eq!(result, TypedExpr::TName("y".to_string(), Type::TInt));
    }

    #[test]
    fn test_eliminate_partial_nested_application() {
        let mut eliminator = PartialElimination::new(false);
        // Before: let f = g 1 in f 2
        // After:  let f = (fun x -> g 1 x) in f 2
        let expr = TypedExpr::TLet(
            "f".to_string(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
            TypedExpr::TApp(
                TypedExpr::TName(
                    "g".to_string(),
                    Type::TFun(
                        Type::TInt.b(),
                        Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                    ),
                )
                .b(),
                vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
            )
            .b(),
            TypedExpr::TApp(
                TypedExpr::TName("f".to_string(), Type::TFun(Type::TInt.b(), Type::TInt.b())).b(),
                vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                Type::TInt,
            )
            .b(),
        );

        let result = eliminator.eliminate_partial(expr);

        match result {
            TypedExpr::TLet(name, typ, box rhs, _body) => {
                assert_eq!(name, "f");
                assert_eq!(typ, Type::TFun(Type::TInt.b(), Type::TInt.b()));

                // Check that rhs is now a lambda
                match rhs {
                    TypedExpr::TLam(args, ..) => {
                        assert_eq!(args.len(), 1);
                        assert_eq!(args[0].1, Type::TInt);
                    }
                    _ => panic!("Expected lambda in let binding rhs"),
                }
            }
            _ => panic!("Expected let binding"),
        }
    }

    #[test]
    fn test_eliminate_partial_multiple_args() {
        let mut eliminator = PartialElimination::new(false);
        // Before: let f = g 1 2 in f 3
        // After:  let f = (fun x -> g 1 2 x) in f 3
        let expr = TypedExpr::TLet(
            "f".to_string(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
            TypedExpr::TApp(
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "g".to_string(),
                        Type::TFun(
                            Type::TInt.b(),
                            Type::TFun(
                                Type::TInt.b(),
                                Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                            )
                            .b(),
                        ),
                    )
                    .b(),
                    vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                    Type::TFun(
                        Type::TInt.b(),
                        Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                    ),
                )
                .b(),
                vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
            )
            .b(),
            TypedExpr::TApp(
                TypedExpr::TName("f".to_string(), Type::TFun(Type::TInt.b(), Type::TInt.b())).b(),
                vec![TypedExpr::TConst(Const::CInt(3), Type::TInt)],
                Type::TInt,
            )
            .b(),
        );

        let result = eliminator.eliminate_partial(expr);

        match result {
            TypedExpr::TLet(_, _, box rhs, _) => match rhs {
                TypedExpr::TLam(args, ..) => {
                    assert_eq!(args.len(), 1);
                }
                _ => panic!("Expected lambda in let binding rhs"),
            },
            _ => panic!("Expected let binding"),
        }
    }

    #[test]
    fn test_eliminate_partial_in_conditional() {
        let mut eliminator = PartialElimination::new(false);
        // Before: if true then (f 1) else (g 2)
        // After:  if true then (f 1) else (g 2) [no change - applications preserved]
        let expr = TypedExpr::TIfThenElse(
            TypedExpr::TConst(Const::CBool(true), Type::TBool).b(),
            TypedExpr::TApp(
                TypedExpr::TName("f".to_string(), Type::TFun(Type::TInt.b(), Type::TInt.b())).b(),
                vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                Type::TInt,
            )
            .b(),
            TypedExpr::TApp(
                TypedExpr::TName("g".to_string(), Type::TFun(Type::TInt.b(), Type::TInt.b())).b(),
                vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                Type::TInt,
            )
            .b(),
            Type::TInt,
        );

        let result = eliminator.eliminate_partial(expr);

        // Applications should be preserved as they're not partial
        match result {
            TypedExpr::TIfThenElse(_, then_branch, else_branch, _) => {
                match (*then_branch, *else_branch) {
                    (TypedExpr::TApp(_, args1, _), TypedExpr::TApp(_, args2, _)) => {
                        assert_eq!(args1.len(), 1);
                        assert_eq!(args2.len(), 1);
                    }
                    _ => panic!("Expected applications in both branches"),
                }
            }
            _ => panic!("Expected if-then-else"),
        }
    }

    #[test]
    fn test_eliminate_partial_nested_let_bindings() {
        let mut eliminator = PartialElimination::new(false);
        // Before: let f = g 1 in let h = f in h 2
        // After:  let f = (fun x -> g 1 x) in f 2
        let expr = TypedExpr::TLet(
            "f".to_string(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
            TypedExpr::TApp(
                TypedExpr::TName(
                    "g".to_string(),
                    Type::TFun(
                        Type::TInt.b(),
                        Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                    ),
                )
                .b(),
                vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
            )
            .b(),
            TypedExpr::TLet(
                "h".to_string(),
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
                TypedExpr::TName("f".to_string(), Type::TFun(Type::TInt.b(), Type::TInt.b())).b(),
                TypedExpr::TApp(
                    TypedExpr::TName("h".to_string(), Type::TFun(Type::TInt.b(), Type::TInt.b()))
                        .b(),
                    vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                    Type::TInt,
                )
                .b(),
            )
            .b(),
        );

        let result = eliminator.eliminate_partial(expr);

        // The inner let binding should be eliminated through substitution
        match result {
            TypedExpr::TLet(name, _, box rhs, box body) => {
                assert_eq!(name, "f");
                match rhs {
                    TypedExpr::TLam(..) => (),
                    _ => panic!("Expected lambda in outer let binding"),
                }
                match body {
                    TypedExpr::TApp(_, _, _) => (),
                    _ => panic!("Expected application after substitution"),
                }
            }
            _ => panic!("Expected let binding"),
        }
    }

    #[test]
    fn test_eliminate_partial_in_lambda_body() {
        let mut eliminator = PartialElimination::new(false);
        // Before: fun x -> (f 1) x
        // After:  fun x -> (f 1) x [no change - application preserved]
        let expr = TypedExpr::TLam(
            vec![("x".to_string(), Type::TInt)],
            TypedExpr::TApp(
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "f".to_string(),
                        Type::TFun(
                            Type::TInt.b(),
                            Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                        ),
                    )
                    .b(),
                    vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                    Type::TFun(Type::TInt.b(), Type::TInt.b()),
                )
                .b(),
                vec![TypedExpr::TName("x".to_string(), Type::TInt)],
                Type::TInt,
            )
            .b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
            None,
        );

        let result = eliminator.eliminate_partial(expr);

        match result {
            TypedExpr::TLam(args, body, ..) => {
                assert_eq!(args.len(), 1);
                match *body {
                    TypedExpr::TApp(_, args, _) => {
                        assert_eq!(args.len(), 1);
                    }
                    _ => panic!("Expected application in lambda body"),
                }
            }
            _ => panic!("Expected lambda"),
        }
    }

    #[test]
    fn test_eliminate_partial_preserves_primitives() {
        let mut eliminator = PartialElimination::new(false);
        // Before: let f = g 1 in f 2 + 3
        // After:  let f = (fun x -> g 1 x) in f 2 + 3
        let expr = TypedExpr::TLet(
            "f".to_string(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
            TypedExpr::TApp(
                TypedExpr::TName(
                    "g".to_string(),
                    Type::TFun(
                        Type::TInt.b(),
                        Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                    ),
                )
                .b(),
                vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
            )
            .b(),
            TypedExpr::TPrim(
                Binop::Add,
                TypedExpr::TApp(
                    TypedExpr::TName("f".to_string(), Type::TFun(Type::TInt.b(), Type::TInt.b()))
                        .b(),
                    vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                    Type::TInt,
                )
                .b(),
                TypedExpr::TConst(Const::CInt(3), Type::TInt).b(),
                Type::TInt,
            )
            .b(),
        );

        let result = eliminator.eliminate_partial(expr);

        match result {
            TypedExpr::TLet(_, _, _, box body) => match body {
                TypedExpr::TPrim(op, _, _, _) => {
                    assert_eq!(op, Binop::Add);
                }
                _ => panic!("Expected primitive operation in body"),
            },
            _ => panic!("Expected let binding"),
        }
    }
}
