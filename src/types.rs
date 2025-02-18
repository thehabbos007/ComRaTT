use std::ops::Deref;

use crate::source::{Binop, Const, Type};

pub type Sym = String;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypedExpr {
    /// A constant value with (the constant, its type)
    TConst(Const, Type),
    /// A variable name with (the name, its type)
    TName(Sym, Type),
    /// A lambda with (args and their types, body expression, the lambda's type)
    TLam(Vec<(Sym, Type)>, Box<TypedExpr>, Type),
    /// A function application with (function expression, argument expressions, result type)
    TApp(Box<TypedExpr>, Vec<TypedExpr>, Type),
    /// A primitive operation with (the operator, left operand, right operand, result type)
    TPrim(Binop, Box<TypedExpr>, Box<TypedExpr>, Type),
    /// A let binding with (bound name, type, bound expression, body expression)
    TLet(Sym, Type, Box<TypedExpr>, Box<TypedExpr>),
    /// A conditional with (condition, then branch, else branch, result type)
    TIfThenElse(Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>, Type),
    /// A tuple with (element expressions, tuple type)
    TTuple(Vec<TypedExpr>, Type),
    /// A tuple access with (tuple expression, index, result type)
    TAccess(Box<TypedExpr>, i32, Type),
}

impl TypedExpr {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TypedToplevel {
    /// A function definition with (name, args and their types, body expression, return type)
    TFunDef(Sym, Vec<(Sym, Type)>, Box<TypedExpr>, Type),
}

impl TypedToplevel {
    pub fn get_type(&self) -> Type {
        match self {
            TypedToplevel::TFunDef(_, _, _, typ) => typ.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypedProg(pub Vec<TypedToplevel>);

impl Deref for TypedProg {
    type Target = Vec<TypedToplevel>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Vec<TypedToplevel>> for TypedProg {
    fn from(v: Vec<TypedToplevel>) -> Self {
        TypedProg(v)
    }
}

#[macro_export]
macro_rules! assert_typed_expr {
    ($expr:expr, $pattern:pat => $assertions:block) => {
        match $expr {
            $pattern => $assertions,
            _ => panic!("Expression did not match expected pattern"),
        }
    };
}

pub fn final_type(ty: &Type) -> Type {
    match ty {
        Type::TInt | Type::TBool | Type::TUnit | Type::TVar(_) => ty.clone(),
        Type::TFun(_, t2) => final_type(t2),
        Type::TProduct(ts) => final_type_tproduct(ts),
    }
}

pub fn final_type_tproduct(ts: &[Type]) -> Type {
    match ts {
        [] => panic!("final_type_tproduct: Attempted to take final type of empty tproduct"),
        [t] => t.clone(),
        [_, rest @ ..] => final_type_tproduct(rest),
    }
}

pub fn tfun_len_n_rec(ty: Type, n: usize, acc: &mut Vec<Type>) -> (Type, Vec<Type>) {
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

pub fn tfun_len_n(ty: Type, n: usize) -> (Type, Vec<Type>) {
    tfun_len_n_rec(ty, n, &mut Vec::new())
}

pub fn build_function_type(types: &[Type], ret_ty: Type) -> Type {
    match types {
        [] => ret_ty,
        [ty, types @ ..] => Type::TFun(ty.clone().b(), build_function_type(types, ret_ty).b()),
    }
}
