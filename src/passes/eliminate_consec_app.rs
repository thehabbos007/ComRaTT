use std::collections::HashMap;

use crate::{
    source::Type,
    types::{unpack_type, TypedExpr, TypedProg, TypedToplevel},
};
use itertools::Itertools;
use map_box::Map;

use super::Pass;

#[derive(Debug, Default)]
pub struct EliminateConsecApp {
    toplevels: Vec<TypedToplevel>,
}

impl Pass for EliminateConsecApp {
    fn run(&mut self, prog: TypedProg) -> TypedProg {
        self.toplevels = prog.0.clone();
        let defs = prog.0;
        let defs = defs
            .into_iter()
            .map(|def| match def {
                TypedToplevel::TFunDef(name, args, body, typ) => TypedToplevel::TFunDef(
                    name,
                    args,
                    body.map_box(|expr| {
                        self.eliminate_consec(expr, HashMap::new())
                            .assert_none_traversal()
                    }),
                    typ,
                ),
                def => def,
            })
            .collect_vec();

        TypedProg(defs, prog.1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TraverseOutcome {
    Take(usize),
    None,
}
impl TraverseOutcome {
    fn new(take: usize) -> Self {
        if take > 0 {
            TraverseOutcome::Take(take)
        } else {
            TraverseOutcome::None
        }
    }

    fn decrement(self) -> Self {
        match self {
            TraverseOutcome::Take(1) => TraverseOutcome::None,
            TraverseOutcome::Take(take) => TraverseOutcome::new(take - 1),
            TraverseOutcome::None => TraverseOutcome::None,
        }
    }
    fn assert_none(self) {
        assert_eq!(self, TraverseOutcome::None)
    }
}

impl TypedExpr {
    fn merge_tapp_args(self, args: Vec<TypedExpr>, typ: Type) -> Self {
        match self {
            TypedExpr::TApp(fun, old_args, _) => {
                TypedExpr::TApp(fun, args.into_iter().chain(old_args).collect(), typ)
            }
            _ => panic!("Expected TApp"),
        }
    }

    fn none_traversal(self) -> (Self, TraverseOutcome) {
        (self, TraverseOutcome::None)
    }
}

trait TraversalExt {
    fn assert_none_traversal(self) -> TypedExpr;
    fn ignore_traversal_outcome(self) -> TypedExpr;
}
impl TraversalExt for (TypedExpr, TraverseOutcome) {
    fn assert_none_traversal(self) -> TypedExpr {
        self.1.assert_none();
        self.0
    }

    fn ignore_traversal_outcome(self) -> TypedExpr {
        self.0
    }
}

impl EliminateConsecApp {
    pub fn new() -> Self {
        Self {
            toplevels: Vec::new(),
        }
    }

    fn eliminate_consec(
        &self,
        expr: TypedExpr,
        mut local_scope: HashMap<String, Type>,
    ) -> (TypedExpr, TraverseOutcome) {
        match expr {
            TypedExpr::TApp(box TypedExpr::TName(ref fun, ref typ), ref args, ..) => {
                let expr = expr.clone();

                let unpacked = unpack_type(typ);
                if !unpacked.is_empty() {
                    // Subtract the applied argument count
                    let take = unpacked.len() - args.len();

                    return (expr, TraverseOutcome::new(take));
                };

                let traverse = self
                    .toplevels
                    .iter()
                    .find_map(|toplevel| match toplevel {
                        TypedToplevel::TFunDef(top_fun, top_args, _, _) if top_fun == fun => {
                            let take = top_args.len().saturating_sub(1);

                            Some(TraverseOutcome::new(take))
                        }
                        _ => None,
                    })
                    .unwrap_or(TraverseOutcome::None);

                (expr, traverse)
            }
            TypedExpr::TApp(box ref inner @ TypedExpr::TApp(..), ref args, ref typ) => {
                let (inner, outcome) = self.eliminate_consec(inner.clone(), local_scope);

                match outcome {
                    TraverseOutcome::Take(take) if take > 0 => (
                        inner.merge_tapp_args(args.clone(), typ.clone()),
                        outcome.decrement(),
                    ),
                    TraverseOutcome::None => {
                        let expr = TypedExpr::TApp(inner.b(), args.clone(), typ.clone());
                        (expr, TraverseOutcome::None)
                    }
                    TraverseOutcome::Take(take) => {
                        panic!("Got take: {take} when eliminating consecutive applications")
                    }
                }
            }
            TypedExpr::TApp(fun_expr, args, typ) => (
                TypedExpr::TApp(
                    fun_expr,
                    args.into_iter()
                        .map(|arg| {
                            self.eliminate_consec(arg, local_scope.clone())
                                .assert_none_traversal()
                        })
                        .collect(),
                    typ,
                ),
                TraverseOutcome::None,
            ),
            TypedExpr::TPrim(op, left, right, typ) => TypedExpr::TPrim(
                op,
                Box::new(
                    self.eliminate_consec(*left, local_scope.clone())
                        .ignore_traversal_outcome(),
                ),
                Box::new(
                    self.eliminate_consec(*right, local_scope)
                        .ignore_traversal_outcome(),
                ),
                typ,
            )
            .none_traversal(),
            TypedExpr::TLet(name, typ, box rhs, body) => {
                let rhs_type = rhs.ty();
                TypedExpr::TLet(
                    name.clone(),
                    typ.clone(),
                    Box::new(
                        self.eliminate_consec(rhs, local_scope.clone())
                            .ignore_traversal_outcome(),
                    ),
                    {
                        local_scope.insert(name, rhs_type);
                        Box::new(
                            self.eliminate_consec(*body, local_scope)
                                .ignore_traversal_outcome(),
                        )
                    },
                )
                .none_traversal()
            }
            TypedExpr::TLam(args, body, typ, clock) => TypedExpr::TLam(
                args,
                Box::new(
                    self.eliminate_consec(*body, local_scope)
                        .ignore_traversal_outcome(),
                ),
                typ,
                clock,
            )
            .none_traversal(),
            TypedExpr::TIfThenElse(condition, then_branch, else_branch, typ) => {
                TypedExpr::TIfThenElse(
                    Box::new(
                        self.eliminate_consec(*condition, local_scope.clone())
                            .ignore_traversal_outcome(),
                    ),
                    Box::new(
                        self.eliminate_consec(*then_branch, local_scope.clone())
                            .ignore_traversal_outcome(),
                    ),
                    Box::new(
                        self.eliminate_consec(*else_branch, local_scope)
                            .ignore_traversal_outcome(),
                    ),
                    typ,
                )
                .none_traversal()
            }
            TypedExpr::TTuple(texps, typ) => TypedExpr::TTuple(
                texps
                    .into_iter()
                    .map(|expr| {
                        self.eliminate_consec(expr, local_scope.clone())
                            .ignore_traversal_outcome()
                    })
                    .collect(),
                typ,
            )
            .none_traversal(),
            TypedExpr::TAccess(texp, idx, typ) => TypedExpr::TAccess(
                Box::new(
                    self.eliminate_consec(*texp, local_scope)
                        .ignore_traversal_outcome(),
                ),
                idx,
                typ,
            )
            .none_traversal(),
            TypedExpr::TConst(_, _) | TypedExpr::TName(_, _) | TypedExpr::TWait(_, _) => {
                expr.none_traversal()
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::source::Type;
    use crate::source::{Binop, Const};

    #[test]
    fn test_eliminate_consecutive_top_level() {
        // bloo: int -> int -> int -> (int -> int)
        // def bloo x y z = fun w -> x + y + z + w
        // main : int -> int
        // def main x = bloo x x x 5
        // Before: (((bloo x) y) z) 5
        // After: bloo(x, y, z) 5
        let mut eliminator = EliminateConsecApp::new();

        let bloo_body = TypedExpr::TLam(
            vec![("w".to_owned(), Type::TInt)],
            Box::new(TypedExpr::TPrim(
                Binop::Add,
                Box::new(TypedExpr::TPrim(
                    Binop::Add,
                    Box::new(TypedExpr::TPrim(
                        Binop::Add,
                        Box::new(TypedExpr::TName("x".to_owned(), Type::TInt)),
                        Box::new(TypedExpr::TName("y".to_owned(), Type::TInt)),
                        Type::TInt,
                    )),
                    Box::new(TypedExpr::TName("z".to_owned(), Type::TInt)),
                    Type::TInt,
                )),
                Box::new(TypedExpr::TName("w".to_owned(), Type::TInt)),
                Type::TInt,
            )),
            Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt)),
            None,
        );

        let bloo_name = TypedExpr::TName(
            "bloo".to_owned(),
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

        let x_var = TypedExpr::TName("x".to_owned(), Type::TInt);
        let five = TypedExpr::TConst(Const::CInt(5), Type::TInt);

        let bloo_x = TypedExpr::TApp(
            Box::new(bloo_name),
            vec![x_var.clone()],
            Type::TFun(
                Box::new(Type::TInt),
                Box::new(Type::TFun(
                    Box::new(Type::TInt),
                    Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
                )),
            ),
        );

        let bloo_x_x = TypedExpr::TApp(
            Box::new(bloo_x),
            vec![x_var.clone()],
            Type::TFun(
                Box::new(Type::TInt),
                Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
            ),
        );

        let bloo_x_x_x = TypedExpr::TApp(
            Box::new(bloo_x_x),
            vec![x_var],
            Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt)),
        );

        let expr = TypedExpr::TApp(Box::new(bloo_x_x_x), vec![five], Type::TInt);

        // Create the top-level program with the bloo function definition
        let toplevel = TypedProg(
            vec![
                TypedToplevel::TFunDef(
                    "bloo".to_owned(),
                    vec![
                        ("x".to_owned(), Type::TInt),
                        ("y".to_owned(), Type::TInt),
                        ("z".to_owned(), Type::TInt),
                    ],
                    bloo_body.b(),
                    Type::TInt,
                ),
                TypedToplevel::TFunDef(
                    "main".to_owned(),
                    vec![("x".to_owned(), Type::TInt)],
                    expr.b(),
                    Type::TInt,
                ),
            ],
            Default::default(),
        );

        let result = eliminator.run(toplevel);

        if let Some(TypedToplevel::TFunDef(name, _, body, _)) = result.0.iter().find(|tl| {
            if let TypedToplevel::TFunDef(n, _, _, _) = tl {
                n == "main"
            } else {
                false
            }
        }) {
            assert_eq!(name, "main");

            match **body {
                TypedExpr::TApp(
                    box TypedExpr::TApp(box TypedExpr::TName(ref func_name, _), ref first_args, _),
                    ref second_args,
                    _,
                ) => {
                    assert_eq!(func_name, "bloo");
                    assert_eq!(first_args.len(), 3);
                    assert_eq!(second_args.len(), 1);
                }
                _ => panic!("Expected flattened application in main function body"),
            }
        } else {
            panic!("Could not find main function in result");
        }
    }
}
