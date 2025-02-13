use std::{any::TypeId, collections::HashMap};

use crate::source::{Binop, Const, Expr, Toplevel, Type};
type Sym = String;

#[derive(PartialEq, Eq, Debug, Clone)]
enum TypedExpr {
    TFunDef(Sym, Vec<(Sym, Type)>, Box<TypedExpr>, Type),
    TConst(Const, Type),
    TName(Sym, Type),
    TLam(Vec<(Sym, Type)>, Box<TypedExpr>, Type),
    TApp(Box<TypedExpr>, Vec<TypedExpr>, Type),
    TPrim(Binop, Box<TypedExpr>, Box<TypedExpr>, Type),
    TLet(Sym, Type, Box<TypedExpr>, Box<TypedExpr>),
    TIfThenElse(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>, Type),
    TTuple(Vec<TypedExpr>, Type),
    TAccess(Box<TypedExpr>, i32, Type),
}

impl TypedExpr {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }
}

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
            Some(_) => panic!(),
            None => None,
        },
        Expr::Prim(op, left, right) => match op {
            Binop::Add | Binop::Mul | Binop::Div | Binop::Sub => todo!(),
            Binop::Lt | Binop::Lte | Binop::Gt | Binop::Gte => todo!(),
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

fn tfun_len_n_rec(ty: Type, n: usize, acc: &mut Vec<Type>) -> (Type, Vec<Type>) {
    match ty {
        Type::TFun(ty, next_ty) if n > 0 => {
            acc.push(*ty);
            tfun_len_n_rec(*next_ty, n - 1, acc)
        }
        t if n == 0 => {
            acc.reverse();
            (t, acc.to_owned())
        }
        Type::TInt | Type::TBool | Type::TUnit => {
            panic!("Attempted to traverse a non-TFun type at n = {}", n)
        }
        _ => panic!("Too many arguments for function"),
    }
}

fn tfun_len_n(ty: Type, n: usize) -> (Type, Vec<Type>) {
    tfun_len_n_rec(ty, n, &mut Vec::new())
}

fn build_function_type(types: &[Type], ret_ty: Type) -> Type {
    match types {
        [] => ret_ty,
        [ty, types @ ..] => Type::TFun(ty.clone().b(), build_function_type(types, ret_ty).b()),
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

fn infer_all(exprs: Vec<Toplevel>) -> Vec<TypedExpr> {
    infer_all_aux(exprs.as_slice(), &mut HashMap::new(), &mut Vec::new())
}

fn infer_all_aux(
    exprs: &[Toplevel],
    context: &mut HashMap<Sym, Type>,
    acc: &mut Vec<TypedExpr>,
) -> Vec<TypedExpr> {
    match exprs {
        [] => {
            acc.reverse();
            acc.to_vec()
        }
        [fexpr, rest @ ..] => match fexpr {
            Toplevel::FunDef(name, box ty @ Type::TFun(_, _), args, body) => {
                let (ret_ty, types) = tfun_len_n(ty.clone(), args.len());
                // args.clone() to avoid borrowing the strings, but is
                // it necessary? is some other way better?
                let args_with_types = args.clone().into_iter().zip(types.clone());
                let mut cloned_context = context.clone();
                cloned_context.insert(name.to_owned(), ty.clone());
                for (arg, typ) in args_with_types.clone() {
                    cloned_context.insert(arg.to_owned(), typ);
                }

                match check(&mut cloned_context, body.clone(), ret_ty) {
                    Some((body_ty, typed_body)) => {
                        let fun_ty = build_function_type(types.as_slice(), body_ty);
                        let typed_fun = TypedExpr::TFunDef(
                            name.to_owned(),
                            args_with_types.collect(),
                            typed_body.b(),
                            fun_ty,
                        );
                        acc.push(typed_fun);
                        infer_all_aux(rest, &mut cloned_context, acc)
                    }
                    None => panic!("Error type checking function {}", name),
                }
            }
            Toplevel::FunDef(name, typ, args, body) if args.len() == 0 => {
                match check(context, body.clone(), *typ.clone()) {
                    Some((fun_ty, typed_body)) => {
                        let typed_fun = TypedExpr::TFunDef(
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
    fn infer_all_constant_function() {
        let fn_type = Type::TInt;
        let fun_body = Expr::Const(Const::CInt(2));
        let fun = Toplevel::FunDef("test".to_owned(), fn_type.b(), vec![], fun_body.b());

        let inferred = infer_all(vec![fun]);
        assert_eq!(inferred.len(), 1);
        match inferred[0].clone() {
            TypedExpr::TFunDef(name, args, box body, ty) => {
                assert_eq!(name, "test");
                assert!(args.is_empty());
                assert_eq!(body, TypedExpr::TConst(Const::CInt(2), Type::TInt));
                assert_eq!(ty, Type::TInt);
            }
            _ => panic!("Not a fundef"), // TODO: assert failure?
        }
    }
}
