#![allow(unused)]

use std::{collections::HashMap, ops::Deref};

use crate::{
    source::{Binop, Const, Expr, Prog, Toplevel, Type},
    types::*,
};

fn get_with_custom_message(opt: Option<(Type, TypedExpr)>, message: String) -> (Type, TypedExpr) {
    match opt {
        Some(value) => value,
        None => panic!("{}", message),
    }
}

fn infer(context: &mut HashMap<Sym, Type>, expr: Box<Expr>) -> Option<(Type, TypedExpr)> {
    match *expr {
        Expr::Const(c) => match c {
            Const::CInt(_) => Some((Type::TInt, TypedExpr::TConst(c, Type::TInt))),
            Const::CBool(_) => Some((Type::TBool, TypedExpr::TConst(c, Type::TBool))),
            Const::CUnit => Some((Type::TUnit, TypedExpr::TConst(c, Type::TUnit))),
        },
        Expr::Var(name) => {
            if let Some(ty) = context.get(&name) {
                return Some((ty.clone(), TypedExpr::TName(name, ty.clone())));
            }
            None
        }
        Expr::Access(expr, idx) => match infer(context, expr) {
            Some((Type::TProduct(types), texp)) if (idx as usize) < types.len() => {
                let typ = types[idx as usize].clone();
                return Some((typ.clone(), TypedExpr::TAccess(texp.b(), idx, typ)));
            }
            _ => None,
        },
        Expr::Tuple(ts) => {
            if ts.len() < 2 {
                return None;
            } else {
                let inferred = ts.into_iter().map(|t| infer(context, t.b()));
                let (types, tyexps): (Vec<Type>, Vec<TypedExpr>) = inferred
                    .map(|opt| {
                        get_with_custom_message(
                            opt,
                            "Failed to infer type of tuple due to element".to_owned(),
                        )
                    })
                    .unzip();
                let tproduct = Type::TProduct(types);
                return Some((tproduct.clone(), TypedExpr::TTuple(tyexps, tproduct)));
            }
        }
        Expr::IfThenElse(condition, then, elseb) => match (
            check(context, condition, Type::TBool),
            infer(context, then),
            infer(context, elseb),
        ) {
            (Some((Type::TBool, tcond)), Some((thenty, tthen)), Some((elsety, telse)))
                if thenty == elsety =>
            {
                Some((
                    thenty.clone(),
                    TypedExpr::TIfThenElse(tcond.b(), tthen.b(), telse.b(), thenty),
                ))
            }
            _ => None,
        },
        Expr::Delay(e) => match infer(context, e) {
            Some((ty, texp)) => Some((
                Type::TFun(Type::TUnit.b(), ty.clone().b()),
                TypedExpr::TLam(
                    vec![("#advance_unit".to_owned(), Type::TUnit)],
                    texp.b(),
                    Type::TFun(Type::TUnit.b(), ty.b()),
                ),
            )),
            None => None,
        },
        Expr::Advance(name) => match context.get(&name) {
            Some(Type::TFun(box Type::TUnit, ty)) => {
                let fun_type = Type::TFun(Type::TUnit.b(), Box::new(*ty.clone()));
                return Some((
                    *ty.clone(),
                    TypedExpr::TApp(
                        TypedExpr::TName(name, fun_type).b(),
                        vec![TypedExpr::TConst(Const::CUnit, Type::TUnit)],
                        *ty.clone(),
                    ),
                ));
            }
            Some(_) => None,
            None => None,
        },
        Expr::Let(name, rhs, body) => match infer(context, rhs) {
            Some((rhs_type, rhs_texp)) => {
                let _ = context.insert(name.clone(), rhs_type);
                match infer(context, body) {
                    Some((body_type, body_texp)) => Some((
                        body_type.clone(),
                        TypedExpr::TLet(name, body_type, rhs_texp.b(), body_texp.b()),
                    )),
                    None => None,
                }
            }
            None => None,
        },
        Expr::App(fun, arg) => match infer(context, fun) {
            Some((Type::TFun(ty, ret_ty), fun_texp)) => match check(context, arg, *ty) {
                Some((_, arg_texp)) => {
                    return Some((
                        *ret_ty.clone(),
                        TypedExpr::TApp(fun_texp.b(), vec![arg_texp], *ret_ty),
                    ))
                }
                None => None,
            },
            Some((ty, _)) => panic!(
                "infer app: Type of function being applied was not TFun but {:?}",
                ty
            ),
            None => None,
        },
        Expr::Prim(op, left, right) => match op {
            Binop::Add | Binop::Mul | Binop::Div | Binop::Sub => match (
                check(context, left, Type::TInt),
                check(context, right, Type::TInt),
            ) {
                (Some((_, tleft)), Some((_, tright))) => Some((
                    Type::TInt,
                    TypedExpr::TPrim(op, tleft.b(), tright.b(), Type::TInt),
                )),
                _ => None,
            },
            Binop::Lt | Binop::Lte | Binop::Gt | Binop::Gte => match (
                check(context, left, Type::TInt),
                check(context, right, Type::TInt),
            ) {
                (Some((_, tleft)), Some((_, tright))) => Some((
                    Type::TBool,
                    TypedExpr::TPrim(op, tleft.b(), tright.b(), Type::TBool),
                )),
                _ => None,
            },
            Binop::Eq | Binop::Neq => match (infer(context, left), infer(context, right)) {
                (Some((Type::TInt, tleft)), Some((Type::TInt, tright)))
                | (Some((Type::TBool, tleft)), Some((Type::TBool, tright))) => Some((
                    Type::TBool,
                    TypedExpr::TPrim(op, tleft.b(), tright.b(), Type::TBool),
                )),
                _ => None,
            },
        },
        _ => panic!(),
    }
}

fn check(context: &mut HashMap<Sym, Type>, expr: Box<Expr>, ty: Type) -> Option<(Type, TypedExpr)> {
    match (expr, ty.clone()) {
        (box Expr::Lam(args, body), Type::TFun(_, _)) => {
            let (ret_ty, types) = tfun_len_n(ty, args.len());
            let args_with_types = args.into_iter().zip(types.clone());
            let mut cloned_context = context.clone();
            for (arg, typ) in args_with_types.clone() {
                cloned_context.insert(arg.to_owned(), typ);
            }
            match check(&mut cloned_context, body, ret_ty.clone()) {
                Some((_, texp)) => {
                    let lambda_type = build_function_type(types.as_slice(), ret_ty);
                    return Some((
                        lambda_type.clone(),
                        TypedExpr::TLam(args_with_types.collect(), texp.b(), lambda_type),
                    ));
                }
                None => None,
            }
        }
        (box Expr::Lam(_, _), _) => None,
        (expr, _) => match infer(context, expr.b()) {
            Some((inferred_ty, texp)) if ty == inferred_ty => Some((inferred_ty, texp)),
            _ => None,
        },
    }
}

pub fn infer_all(prog: Prog) -> TypedProg {
    let toplevels = prog.0;
    let typed_toplevels = infer_all_aux(toplevels.as_slice(), &mut HashMap::new(), Vec::new());

    TypedProg(typed_toplevels)
}

fn infer_all_aux(
    toplevels: &[Toplevel],
    context: &mut HashMap<Sym, Type>,
    mut acc: Vec<TypedToplevel>,
) -> Vec<TypedToplevel> {
    match toplevels {
        [] => {
            acc.reverse();
            acc.to_vec()
        }
        [fexpr, rest @ ..] => match fexpr {
            Toplevel::FunDef(name, ty @ Type::TFun(_, _), args, body) => {
                let (ret_ty, types) = tfun_len_n(ty.clone(), args.len());
                // args.clone() to avoid borrowing the strings, but is
                // it necessary? is some other way better?
                let args_with_types = args.clone().into_iter().zip(types.clone());
                let mut cloned_context = context.clone();
                cloned_context.insert(name.to_owned(), ty.clone());
                for (arg, typ) in args_with_types.clone() {
                    cloned_context.insert(arg.to_owned(), typ);
                }

                match check(&mut cloned_context, body.clone(), ret_ty.clone()) {
                    Some((body_ty, typed_body)) => {
                        let typed_fun = TypedToplevel::TFunDef(
                            name.to_owned(),
                            args_with_types.collect(),
                            typed_body.b(),
                            ret_ty,
                        );
                        acc.push(typed_fun);
                        infer_all_aux(rest, &mut cloned_context, acc)
                    }
                    None => panic!("Error type checking function {}", name),
                }
            }
            Toplevel::FunDef(name, typ, args, body) if args.len() == 0 => {
                match check(context, body.clone(), typ.clone()) {
                    Some((fun_ty, typed_body)) => {
                        let typed_fun = TypedToplevel::TFunDef(
                            name.to_owned(),
                            vec![],
                            typed_body.b(),
                            fun_ty.clone(),
                        );
                        let mut cloned_context = context.clone();
                        cloned_context.insert(name.to_owned(), fun_ty);
                        acc.push(typed_fun);
                        infer_all_aux(rest, &mut cloned_context, acc)
                    }
                    None => panic!("Error type checking function {} with no arguments", name),
                }
            }
            _ => panic!("Error: Non-annotated function"),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_function_type_returns_correct_type() {
        let expected_type = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TBool.b(), Type::TBool.b()).b(),
        );
        let arg_types = vec![Type::TInt, Type::TBool];
        let ret_ty = Type::TBool;
        let actual_type = build_function_type(arg_types.as_slice(), ret_ty);
        assert_eq!(actual_type, expected_type);
    }

    #[test]
    fn infer_all_function_with_shadowing() {
        let fun_type = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let fun_args = vec!["x".to_owned()];
        let fun_body = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CInt(40)).b(),
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Const(Const::CInt(2)).b(),
            )
            .b(),
        );
        let fun = Toplevel::FunDef(
            "test".to_owned(),
            fun_type.clone(),
            fun_args,
            fun_body.clone().b(),
        );
        let expected_body = TypedExpr::TLet(
            "x".to_owned(),
            Type::TInt,
            TypedExpr::TConst(Const::CInt(40), Type::TInt).b(),
            TypedExpr::TPrim(
                Binop::Add,
                TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
                Type::TInt,
            )
            .b(),
        );
        let inferred = infer_all(vec![fun].into());
        assert_eq!(inferred.len(), 1);
        match inferred[0].clone() {
            TypedToplevel::TFunDef(name, args, box body, ty) => {
                assert_eq!(name, "test".to_owned());
                assert_eq!(args, vec![("x".to_owned(), Type::TInt)]);
                assert_eq!(body, expected_body);
                assert_eq!(ty, fun_type)
            }
        }
    }

    #[test]
    fn infer_all_function_with_shadowing_where_types_change() {
        let fun_type = Type::TFun(Type::TInt.b(), Type::TBool.b());
        let fun_args = vec!["x".to_owned()];
        let fun_body = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CBool(true)).b(),
            Expr::Var("x".to_owned()).b(),
        );
        let fun = Toplevel::FunDef(
            "test".to_owned(),
            fun_type.clone(),
            fun_args,
            fun_body.clone().b(),
        );
        let expected_body = TypedExpr::TLet(
            "x".to_owned(),
            Type::TBool,
            TypedExpr::TConst(Const::CBool(true), Type::TBool).b(),
            TypedExpr::TName("x".to_owned(), Type::TBool).b(),
        );
        let inferred = infer_all(vec![fun].into());
        assert_eq!(inferred.len(), 1);
        match inferred[0].clone() {
            TypedToplevel::TFunDef(name, args, box body, ty) => {
                assert_eq!(name, "test".to_owned());
                assert_eq!(args, vec![("x".to_owned(), Type::TInt)]);
                assert_eq!(body, expected_body);
                assert_eq!(ty, fun_type)
            }
        }
    }

    #[test]
    fn infer_all_constant_function() {
        let fn_type = Type::TInt;
        let fun_body = Expr::Const(Const::CInt(2));
        let fun = Toplevel::FunDef("test".to_owned(), fn_type, vec![], fun_body.b());

        let inferred = infer_all(vec![fun].into());
        assert_eq!(inferred.len(), 1);
        match inferred[0].clone() {
            TypedToplevel::TFunDef(name, args, box body, ty) => {
                assert_eq!(name, "test");
                assert!(args.is_empty());
                assert_eq!(body, TypedExpr::TConst(Const::CInt(2), Type::TInt));
                assert_eq!(ty, Type::TInt);
            }
        }
    }

    #[test]
    fn infer_int_const() {
        let expr = Expr::Const(Const::CInt(42));
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(texp, TypedExpr::TConst(Const::CInt(42), Type::TInt));
            }
            None => panic!("Failed to infer type of int constant 42"),
        }
    }

    #[test]
    fn infer_tuple_nested_tuple_access() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Tuple(vec![
                Expr::Const(Const::CBool(true)),
                Expr::Const(Const::CBool(false)),
            ]),
        ]);
        let outer_access = Expr::Access(tuple.b(), 1);
        let inner_access = Expr::Access(outer_access.b(), 0);
        let inner_tuple_type = Type::TProduct(vec![Type::TBool, Type::TBool]);
        let outer_tuple_type = Type::TProduct(vec![Type::TInt, inner_tuple_type.clone()]);
        let expected_texp = TypedExpr::TAccess(
            TypedExpr::TAccess(
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TTuple(
                            vec![
                                TypedExpr::TConst(Const::CBool(true), Type::TBool),
                                TypedExpr::TConst(Const::CBool(false), Type::TBool),
                            ],
                            inner_tuple_type.clone(),
                        ),
                    ],
                    outer_tuple_type,
                )
                .b(),
                1,
                inner_tuple_type,
            )
            .b(),
            0,
            Type::TBool,
        );
        let inferred = infer(&mut HashMap::new(), inner_access.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TBool);
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of nested tuple access"),
        }
    }

    #[test]
    fn infer_valid_tuple_access() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expr = Expr::Access(tuple.b(), 0);
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(
                    texp,
                    TypedExpr::TAccess(
                        TypedExpr::TTuple(
                            vec![
                                TypedExpr::TConst(Const::CInt(42), Type::TInt),
                                TypedExpr::TConst(Const::CBool(true), Type::TBool)
                            ],
                            Type::TProduct(vec![Type::TInt, Type::TBool])
                        )
                        .b(),
                        0,
                        Type::TInt
                    )
                );
            }
            None => panic!("Failed to infer type of valid tuple access"),
        }
    }

    #[test]
    fn infer_sub_zero_tuple_access_should_fail() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expr = Expr::Access(tuple.b(), -1);
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some(_) => panic!("Should have failed to infer type of sub-zero tuple access"),
            None => (),
        }
    }

    #[test]
    fn infer_advance_name_bound_to_thunk_in_context() {
        let expr = Expr::Advance("x".to_owned());
        let mut context = HashMap::new();
        let fun_type = Type::TFun(Type::TUnit.b(), Type::TInt.b());
        context.insert("x".to_owned(), fun_type.clone());
        let expected_texp = TypedExpr::TApp(
            TypedExpr::TName("x".to_owned(), fun_type).b(),
            vec![TypedExpr::TConst(Const::CUnit, Type::TUnit)],
            Type::TInt,
        );
        let inferred = infer(&mut context, expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of valid advance"),
        }
    }

    #[test]
    fn infer_advance_name_not_bound_in_context_should_fail() {
        let expr = Expr::Advance("x".to_owned());
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some(_) => panic!("Should have failed to infer type of advance on unbound name"),
            None => (),
        }
    }

    #[test]
    fn infer_advance_name_not_bound_to_thunk_in_context_should_fail() {
        let expr = Expr::Advance("x".to_owned());
        let mut context = HashMap::new();
        context.insert("x".to_owned(), Type::TInt);
        let inferred = infer(&mut context, expr.b());
        match inferred {
            Some(_) => {
                panic!("Should have failed to infer type of advance on name not bound to thunk")
            }
            None => (),
        }
    }

    #[test]
    fn infer_delay_produces_thunk() {
        let expr = Expr::Delay(Expr::Const(Const::CInt(42)).b());
        let inferred = infer(&mut HashMap::new(), expr.b());
        let expected_texp = TypedExpr::TLam(
            vec![("#advance_unit".to_owned(), Type::TUnit)],
            TypedExpr::TConst(Const::CInt(42), Type::TInt).b(),
            Type::TFun(Type::TUnit.b(), Type::TInt.b()),
        );
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TFun(Type::TUnit.b(), Type::TInt.b()));
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of delay"),
        }
    }

    #[test]
    fn infer_out_of_bounds_tuple_access_should_fail() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expr = Expr::Access(tuple.b(), 2);
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some(_) => panic!("Should have failed to infer type of out-of-bounds tuple access"),
            None => (),
        }
    }

    #[test]
    fn infer_all_tuple_access_used_in_primitive_op_in_function() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
            Expr::Const(Const::CInt(40)),
        ]);
        let fun_body = Expr::Prim(
            Binop::Add,
            Expr::Var("x".to_owned()).b(),
            Expr::Access(tuple.b(), 2).b(),
        );
        let fun_type = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let fun_args = vec!["x".to_owned()];
        let fun = Toplevel::FunDef("test".to_owned(), fun_type.clone(), fun_args, fun_body.b());
        let expected_body = TypedExpr::TPrim(
            Binop::Add,
            TypedExpr::TName("x".to_owned(), Type::TInt).b(),
            TypedExpr::TAccess(
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TConst(Const::CBool(true), Type::TBool),
                        TypedExpr::TConst(Const::CInt(40), Type::TInt),
                    ],
                    Type::TProduct(vec![Type::TInt, Type::TBool, Type::TInt]),
                )
                .b(),
                2,
                Type::TInt,
            )
            .b(),
            Type::TInt,
        );
        let inferred = infer_all(vec![fun].into());
        assert_eq!(inferred.len(), 1);
        match inferred[0].clone() {
            TypedToplevel::TFunDef(name, args, box body, ty) => {
                assert_eq!(name, "test");
                assert_eq!(args.len(), 1);
                assert_eq!(body, expected_body);
                assert_eq!(ty, fun_type);
            }
            _ => panic!("Failed to infer of function that adds argument to tuple access"),
        }
    }

    #[test]
    fn infer_let_bound_tuple_accessed_in_body() {
        let tuple = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
            Expr::Const(Const::CBool(false)),
        ]);
        let expr = Expr::Let(
            "tuple_binding".to_owned(),
            tuple.b(),
            Expr::Access(Expr::Var("tuple_binding".to_owned()).b(), 0).b(),
        );
        let tuple_type = Type::TProduct(vec![Type::TInt, Type::TBool, Type::TBool]);
        let tuple_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CInt(42), Type::TInt),
                TypedExpr::TConst(Const::CBool(true), Type::TBool),
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
            ],
            tuple_type.clone(),
        );
        let expected_texp = TypedExpr::TLet(
            "tuple_binding".to_owned(),
            Type::TInt,
            tuple_texp.b(),
            TypedExpr::TAccess(
                TypedExpr::TName("tuple_binding".to_owned(), tuple_type.clone()).b(),
                0,
                Type::TInt,
            )
            .b(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
          Some((ty, texp)) => {
            assert_eq!(ty, Type::TInt);
            assert_eq!(texp, expected_texp);
          }
          None => panic!("Failed to infer type of let binding where rhs is a tuple and body accesses it by name"),
        }
    }

    #[test]
    fn infer_valid_two_element_tuple_with_second_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Const(Const::CBool(false)),
            Expr::Tuple(vec![
                Expr::Const(Const::CInt(42)),
                Expr::Const(Const::CBool(true)),
            ]),
        ]);
        let expected_second_element_type = Type::TProduct(vec![Type::TInt, Type::TBool]);
        let expected_type = Type::TProduct(vec![Type::TBool, expected_second_element_type.clone()]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TConst(Const::CBool(true), Type::TBool),
                    ],
                    expected_second_element_type,
                ),
            ],
            expected_type.clone(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, expected_type);
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of valid tuple (42, (true, false))"),
        }
    }

    #[test]
    fn infer_valid_two_element_tuple_with_first_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Tuple(vec![
                Expr::Const(Const::CInt(42)),
                Expr::Const(Const::CBool(true)),
            ]),
            Expr::Const(Const::CBool(false)),
        ]);
        let expected_first_element_type = Type::TProduct(vec![Type::TInt, Type::TBool]);
        let expected_type = Type::TProduct(vec![expected_first_element_type.clone(), Type::TBool]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TTuple(
                    vec![
                        TypedExpr::TConst(Const::CInt(42), Type::TInt),
                        TypedExpr::TConst(Const::CBool(true), Type::TBool),
                    ],
                    expected_first_element_type,
                ),
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
            ],
            expected_type.clone(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, expected_type);
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of valid tuple ((42, true), false)"),
        }
    }

    #[test]
    fn infer_valid_three_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
            Expr::Const(Const::CBool(false)),
        ]);
        let expected_type = Type::TProduct(vec![Type::TInt, Type::TBool, Type::TBool]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CInt(42), Type::TInt),
                TypedExpr::TConst(Const::CBool(true), Type::TBool),
                TypedExpr::TConst(Const::CBool(false), Type::TBool),
            ],
            expected_type.clone(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, expected_type);
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of valid tuple (42, true, false)"),
        }
    }

    #[test]
    fn infer_valid_two_element_tuple() {
        let expr = Expr::Tuple(vec![
            Expr::Const(Const::CInt(42)),
            Expr::Const(Const::CBool(true)),
        ]);
        let expected_type = Type::TProduct(vec![Type::TInt, Type::TBool]);
        let expected_texp = TypedExpr::TTuple(
            vec![
                TypedExpr::TConst(Const::CInt(42), Type::TInt),
                TypedExpr::TConst(Const::CBool(true), Type::TBool),
            ],
            expected_type.clone(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, expected_type);
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of valid tuple (42, true)"),
        }
    }

    #[test]
    fn infer_single_element_tuple_should_fail() {
        let expr = Expr::Tuple(vec![Expr::Const(Const::CInt(42))]);
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some(_) => panic!("Should have failed to infer type of single element tuple"),
            None => (),
        }
    }

    #[test]
    fn infer_zero_element_tuple_should_fail() {
        let expr = Expr::Tuple(vec![]);
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some(_) => panic!("Should have failed to infer type of zero element tuple"),
            None => (),
        }
    }

    #[test]
    fn infer_conditional_different_branch_types_should_fail() {
        let expr = Expr::IfThenElse(
            Expr::Const(Const::CBool(true)).b(),
            Expr::Const(Const::CInt(42)).b(),
            Expr::Const(Const::CBool(false)).b(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some(_) => panic!(
                "Should have failed to infer type of conditional with different branch types"
            ),
            None => (),
        }
    }
    #[test]
    fn infer_conditional() {
        let expr = Expr::IfThenElse(
            Expr::Const(Const::CBool(true)).b(),
            Expr::Const(Const::CInt(42)).b(),
            Expr::Const(Const::CInt(0)).b(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(
                    texp,
                    TypedExpr::TIfThenElse(
                        TypedExpr::TConst(Const::CBool(true), Type::TBool).b(),
                        TypedExpr::TConst(Const::CInt(42), Type::TInt).b(),
                        TypedExpr::TConst(Const::CInt(0), Type::TInt).b(),
                        Type::TInt
                    )
                );
            }
            None => panic!("Failed to infer type of valid conditional"),
        }
    }

    #[test]
    fn infer_var_in_context() {
        let expr = Expr::Var("x".to_owned());
        let context = &mut HashMap::new();
        context.insert("x".to_owned(), Type::TInt);
        let inferred = infer(context, expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(texp, TypedExpr::TName("x".to_owned(), Type::TInt));
            }
            None => panic!("Failed to infer type of variable that has type in context"),
        }
    }

    #[test]
    fn infer_var_not_in_context_should_fail() {
        let expr = Expr::Var("x".to_owned());
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some(_) => {
                panic!("Should have failed to infer type of variable that has no type in context")
            }
            None => (),
        }
    }

    #[test]
    fn infer_let_binding_with_prim_body() {
        let expr = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CInt(2)).b(),
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Var("x".to_owned()).b(),
            )
            .b(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(
                    texp,
                    TypedExpr::TLet(
                        "x".to_owned(),
                        Type::TInt,
                        TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
                        TypedExpr::TPrim(
                            Binop::Add,
                            TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                            TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                            Type::TInt
                        )
                        .b()
                    )
                );
            }
            None => panic!("Failed to infer type of 'let x = 2 in x+x'"),
        }
    }

    #[test]
    fn infer_let_binding_with_rhs_as_body() {
        let expr = Expr::Let(
            "x".to_owned(),
            Expr::Const(Const::CInt(42)).b(),
            Expr::Var("x".to_owned()).b(),
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(
                    texp,
                    TypedExpr::TLet(
                        "x".to_owned(),
                        Type::TInt,
                        TypedExpr::TConst(Const::CInt(42), Type::TInt).b(),
                        TypedExpr::TName("x".to_owned(), Type::TInt).b()
                    )
                );
            }
            None => panic!("Failed to infer type of 'let x = 42 in x'"),
        }
    }

    #[test]
    fn infer_prim_add() {
        let expr = Expr::Prim(
            Binop::Add,
            Expr::Const(Const::CInt(40)).b(),
            Expr::Const(Const::CInt(2)).b(),
        );
        let expected_texp = TypedExpr::TPrim(
            Binop::Add,
            TypedExpr::TConst(Const::CInt(40), Type::TInt).b(),
            TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
            Type::TInt,
        );
        let inferred = infer(&mut HashMap::new(), expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to infer type of primitive expression '40 + 2'"),
        }
    }

    #[test]
    fn check_lambda_against_non_tfun_should_fail() {
        let expr = Expr::Lam(
            vec!["x".to_owned()],
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Const(Const::CInt(2)).b(),
            )
            .b(),
        );
        let checked = check(&mut HashMap::new(), expr.b(), Type::TInt);
        match checked {
            Some(_) => panic!("Should have failed to check type of lambda against non TFun"),
            None => (),
        }
    }

    #[test]
    fn check_valid_lambda() {
        let expr = Expr::Lam(
            vec!["x".to_owned()],
            Expr::Prim(
                Binop::Add,
                Expr::Var("x".to_owned()).b(),
                Expr::Const(Const::CInt(2)).b(),
            )
            .b(),
        );
        let expected_texp = TypedExpr::TLam(
            vec![("x".to_owned(), Type::TInt)],
            TypedExpr::TPrim(
                Binop::Add,
                TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                TypedExpr::TConst(Const::CInt(2), Type::TInt).b(),
                Type::TInt,
            )
            .b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
        );
        let checked = check(
            &mut HashMap::new(),
            expr.b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
        );
        match checked {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TFun(Type::TInt.b(), Type::TInt.b()));
                assert_eq!(texp, expected_texp);
            }
            None => panic!("Failed to check type of valid lambda 'fun x -> x+2'"),
        }
    }

    #[test]
    fn infer_valid_multiple_application() {
        let expr = Expr::App(
            Expr::App(
                Expr::Var("f".to_owned()).b(),
                Expr::Const(Const::CInt(2)).b(),
            )
            .b(),
            Expr::Const(Const::CInt(2)).b(),
        );
        let inner_fun_type = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
        );
        let mut context = HashMap::from([("f".to_owned(), inner_fun_type.clone())]);
        let expected_inner_fun = TypedExpr::TName("f".to_owned(), inner_fun_type.clone());
        let expected_inner_app = TypedExpr::TApp(
            expected_inner_fun.clone().b(),
            vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
            Type::TFun(Type::TInt.b(), Type::TInt.b()),
        );

        let expected_outer_app = TypedExpr::TApp(
            expected_inner_app.clone().b(),
            vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
            Type::TInt,
        );
        let inferred = infer(&mut context, expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(texp, expected_outer_app);
            }
            None => panic!("Failed to infer type of valid multiple application"),
        }
    }

    #[test]
    fn infer_invalid_application_should_fail() {
        let expr = Expr::App(
            Expr::Var("f".to_owned()).b(),
            Expr::Const(Const::CInt(42)).b(),
        );
        let mut context = HashMap::new();
        context.insert("f".to_owned(), Type::TFun(Type::TBool.b(), Type::TInt.b()));
        let inferred = infer(&mut context, expr.b());
        match inferred {
            Some(_) => panic!("Should have failed to infer type of invalid application"),
            None => (),
        }
    }
    #[test]
    fn infer_valid_application() {
        let expr = Expr::App(
            Expr::Var("f".to_owned()).b(),
            Expr::Const(Const::CInt(42)).b(),
        );
        let mut context = HashMap::new();
        context.insert("f".to_owned(), Type::TFun(Type::TInt.b(), Type::TInt.b()));
        let inferred = infer(&mut context, expr.b());
        match inferred {
            Some((ty, texp)) => {
                assert_eq!(ty, Type::TInt);
                assert_eq!(
                    texp,
                    TypedExpr::TApp(
                        TypedExpr::TName(
                            "f".to_owned(),
                            Type::TFun(Type::TInt.b(), Type::TInt.b())
                        )
                        .b(),
                        vec![TypedExpr::TConst(Const::CInt(42), Type::TInt)],
                        Type::TInt
                    )
                );
            }
            None => panic!("Failed to infer type of valid application"),
        }
    }

    #[test]
    #[should_panic]
    fn tfun_len_n_given_non_tfun_panics() {
        tfun_len_n(Type::TBool, 1);
    }

    #[test]
    #[should_panic]
    fn tfun_len_n_given_tfun_and_out_of_bounds_n_panics() {
        tfun_len_n(Type::TFun(Type::TInt.b(), Type::TInt.b()), 2);
    }

    #[test]
    fn tfun_len_n_given_tfun_and_zero_returns_type_unmodified() {
        let tfun = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let (ret_ty, types) = tfun_len_n(tfun.clone(), 0);
        assert_eq!(ret_ty, tfun);
        assert_eq!(types, vec![]);
    }

    #[test]
    fn tfun_len_n_given_singlearg_tfun_and_n_equals_length_of_args() {
        let tfun = Type::TFun(Type::TInt.b(), Type::TInt.b());
        let (ret_ty, types) = tfun_len_n(tfun.clone(), 1);
        assert_eq!(ret_ty, Type::TInt);
        assert_eq!(types, vec![Type::TInt]);
    }

    #[test]
    fn tfun_len_n_given_multiarg_tfun_and_n_equals_length_of_args() {
        let tfun = Type::TFun(
            Type::TInt.b(),
            Type::TFun(Type::TInt.b(), Type::TBool.b()).b(),
        );
        let (ret_ty, types) = tfun_len_n(tfun.clone(), 2);
        assert_eq!(ret_ty, Type::TBool);
        assert_eq!(types, vec![Type::TInt, Type::TInt]);
    }
}
