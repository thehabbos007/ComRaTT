use crate::infer::{TypedExpr, TypedProg, TypedToplevel};
use crate::source::Type;
use itertools::Itertools;
use map_box::Map;

use super::Pass;

fn final_type(ty: &Type) -> Type {
    match ty {
        Type::TInt | Type::TBool | Type::TUnit => ty.clone(),
        Type::TFun(_, t2) => final_type(t2),
        Type::TProduct(ts) => final_type_tproduct(ts),
    }
}

fn final_type_tproduct(ts: &[Type]) -> Type {
    match ts {
        [] => panic!("final_type_tproduct: Attempted to take final type of empty tproduct"),
        [t] => t.clone(),
        [_, rest @ ..] => final_type_tproduct(rest),
    }
}

#[derive(Debug, Default)]
pub struct EliminateConsecApp;

impl Pass for EliminateConsecApp {
    fn run(&mut self, prog: TypedProg) -> TypedProg {
        let defs = prog.0;
        let defs = defs
            .into_iter()
            .map(|TypedToplevel::TFunDef(name, args, body, typ)| {
                TypedToplevel::TFunDef(name, args, body.map_box(|b| self.eliminate_consec(b)), typ)
            })
            .collect_vec();

        defs.into()
    }
}

impl EliminateConsecApp {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn eliminate_consec(&mut self, expr: TypedExpr) -> TypedExpr {
        match expr {
            TypedExpr::TConst(_, _) => expr,
            TypedExpr::TName(_, _) => expr,
            TypedExpr::TPrim(op, left, right, typ) => TypedExpr::TPrim(
                op,
                Box::new(self.eliminate_consec(*left)),
                Box::new(self.eliminate_consec(*right)),
                typ,
            ),
            TypedExpr::TLet(name, typ, rhs, body) => TypedExpr::TLet(
                name,
                typ,
                Box::new(self.eliminate_consec(*rhs)),
                Box::new(self.eliminate_consec(*body)),
            ),
            // An application where the "body" is itself an application
            TypedExpr::TApp(box TypedExpr::TApp(box f, mut args1, ty1), mut args2, _) => {
                args1.append(&mut args2);
                let resulting_expr = TypedExpr::TApp(f.b(), args1, final_type(&ty1));
                self.eliminate_consec(resulting_expr)
            }
            TypedExpr::TApp(fn_expr, args, typ) => TypedExpr::TApp(
                Box::new(self.eliminate_consec(*fn_expr)),
                args.into_iter()
                    .map(|arg| self.eliminate_consec(arg))
                    .collect(),
                typ,
            ),
            TypedExpr::TLam(args, body, typ) => {
                TypedExpr::TLam(args, Box::new(self.eliminate_consec(*body)), typ)
            }
            TypedExpr::TIfThenElse(condition, then_branch, else_branch, typ) => {
                TypedExpr::TIfThenElse(
                    Box::new(self.eliminate_consec(*condition)),
                    Box::new(self.eliminate_consec(*then_branch)),
                    Box::new(self.eliminate_consec(*else_branch)),
                    typ,
                )
            }
            TypedExpr::TTuple(texps, typ) => TypedExpr::TTuple(
                texps
                    .into_iter()
                    .map(|texp| self.eliminate_consec(texp))
                    .collect(),
                typ,
            ),
            TypedExpr::TAccess(texp, idx, typ) => {
                TypedExpr::TAccess(Box::new(self.eliminate_consec(*texp)), idx, typ)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Binop, Const};

    #[test]
    fn test_eliminate_consecutive_simple() {
        // Before: (f 1) 2
        // After: f 1 2
        let mut eliminator = EliminateConsecApp::new();
        let expr = TypedExpr::TApp(
            TypedExpr::TApp(
                TypedExpr::TName(
                    "f".to_owned(),
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
            vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
            Type::TInt,
        );

        let result = eliminator.eliminate_consec(expr);

        match result {
            TypedExpr::TApp(box TypedExpr::TName(name, _), args, typ) => {
                assert_eq!(name, "f");
                assert_eq!(args.len(), 2);
                assert_eq!(typ, Type::TInt);
            }
            _ => panic!("Expected flattened application"),
        }
    }

    #[test]
    fn test_eliminate_consecutive_multiple() {
        // Before: ((f 1) 2) 3
        // After: f 1 2 3
        let mut eliminator = EliminateConsecApp::new();
        let expr = TypedExpr::TApp(
            TypedExpr::TApp(
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "f".to_owned(),
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
            vec![TypedExpr::TConst(Const::CInt(3), Type::TInt)],
            Type::TInt,
        );

        let result = eliminator.eliminate_consec(expr);

        match result {
            TypedExpr::TApp(box TypedExpr::TName(name, _), args, typ) => {
                assert_eq!(name, "f");
                assert_eq!(args.len(), 3);
                assert_eq!(typ, Type::TInt);
            }
            _ => panic!("Expected flattened application"),
        }
    }

    #[test]
    fn test_eliminate_consecutive_in_let() {
        // Before: let x = (f 1) 2 in x
        // After: let x = f 1 2 in x
        let mut eliminator = EliminateConsecApp::new();
        let expr = TypedExpr::TLet(
            "x".to_owned(),
            Type::TInt,
            TypedExpr::TApp(
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "f".to_owned(),
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
                vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                Type::TInt,
            )
            .b(),
            TypedExpr::TName("x".to_owned(), Type::TInt).b(),
        );

        let result = eliminator.eliminate_consec(expr);

        match result {
            TypedExpr::TLet(_, _, box rhs, _) => match rhs {
                TypedExpr::TApp(box TypedExpr::TName(name, _), args, typ) => {
                    assert_eq!(name, "f");
                    assert_eq!(args.len(), 2);
                    assert_eq!(typ, Type::TInt);
                }
                _ => panic!("Expected flattened application in let binding"),
            },
            _ => panic!("Expected let binding"),
        }
    }

    #[test]
    fn test_eliminate_consecutive_in_if() {
        // Before: if (f 1) 2 then true else false
        // After: if f 1 2 then true else false
        let mut eliminator = EliminateConsecApp::new();
        let expr = TypedExpr::TIfThenElse(
            TypedExpr::TApp(
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "f".to_owned(),
                        Type::TFun(
                            Type::TInt.b(),
                            Type::TFun(Type::TInt.b(), Type::TBool.b()).b(),
                        ),
                    )
                    .b(),
                    vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                    Type::TFun(Type::TInt.b(), Type::TBool.b()),
                )
                .b(),
                vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                Type::TBool,
            )
            .b(),
            TypedExpr::TConst(Const::CBool(true), Type::TBool).b(),
            TypedExpr::TConst(Const::CBool(false), Type::TBool).b(),
            Type::TBool,
        );

        let result = eliminator.eliminate_consec(expr);

        match result {
            TypedExpr::TIfThenElse(box condition, _, _, _) => match condition {
                TypedExpr::TApp(box TypedExpr::TName(name, _), args, typ) => {
                    assert_eq!(name, "f");
                    assert_eq!(args.len(), 2);
                    assert_eq!(typ, Type::TBool);
                }
                _ => panic!("Expected flattened application in condition"),
            },
            _ => panic!("Expected if expression"),
        }
    }

    #[test]
    fn test_eliminate_consecutive_preserves_primitives() {
        // Before: (f 1) 2 + 3
        // After: f 1 2 + 3
        let mut eliminator = EliminateConsecApp::new();
        let expr = TypedExpr::TPrim(
            Binop::Add,
            TypedExpr::TApp(
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "f".to_owned(),
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
                vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                Type::TInt,
            )
            .b(),
            TypedExpr::TConst(Const::CInt(3), Type::TInt).b(),
            Type::TInt,
        );

        let result = eliminator.eliminate_consec(expr);

        match result {
            TypedExpr::TPrim(op, left, _, _) => {
                assert_eq!(op, Binop::Add);
                match *left {
                    TypedExpr::TApp(box TypedExpr::TName(name, _), args, typ) => {
                        assert_eq!(name, "f");
                        assert_eq!(args.len(), 2);
                        assert_eq!(typ, Type::TInt);
                    }
                    _ => panic!("Expected flattened application on left side of primitive"),
                }
            }
            _ => panic!("Expected primitive operation"),
        }
    }

    #[test]
    fn test_eliminate_consecutive_in_lambda() {
        // Before: fun x -> (f 1) 2
        // After: fun x -> f 1 2
        let mut eliminator = EliminateConsecApp::new();
        let expr = TypedExpr::TLam(
            vec![("x".to_owned(), Type::TInt)],
            TypedExpr::TApp(
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "f".to_owned(),
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
                vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                Type::TInt,
            )
            .b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
        );

        let result = eliminator.eliminate_consec(expr);

        match result {
            TypedExpr::TLam(_, box body, _) => match body {
                TypedExpr::TApp(box TypedExpr::TName(name, _), args, typ) => {
                    assert_eq!(name, "f");
                    assert_eq!(args.len(), 2);
                    assert_eq!(typ, Type::TInt);
                }
                _ => panic!("Expected flattened application in lambda body"),
            },
            _ => panic!("Expected lambda"),
        }
    }
}
