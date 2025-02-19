use crate::source::{Binop, Const, Type};
use crate::types::{Sym, TypedExpr, TypedProg, TypedToplevel};
use std::ops::Deref;

type Param = (Sym, Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AExpr {
    Const(Const, Type),
    Var(Sym, Type),
    Lam(Vec<Param>, Box<AnfExpr>),
}

impl AExpr {
    pub fn ty(&self) -> Type {
        match self {
            AExpr::Const(_, ty) => ty.clone(),
            AExpr::Var(_, ty) => ty.clone(),
            AExpr::Lam(_, _) => Type::TUnit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CExpr {
    Prim(Binop, AExpr, AExpr, Type),
    App(AExpr, Vec<AExpr>, Type),
    Tuple(Vec<AExpr>, Type),
    Access(AExpr, i32, Type),
    IfThenElse(AExpr, Box<AnfExpr>, Box<AnfExpr>, Type),
}

impl CExpr {
    pub fn ty(&self) -> Type {
        match self {
            CExpr::Prim(_, _, _, ty) => ty.clone(),
            CExpr::App(_, _, ty) => ty.clone(),
            CExpr::Tuple(_, ty) => ty.clone(),
            CExpr::Access(_, _, ty) => ty.clone(),
            CExpr::IfThenElse(_, _, _, ty) => ty.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnfExpr {
    AExpr(AExpr),
    CExp(CExpr),
    Let(Sym, Type, Box<AnfExpr>, Box<AnfExpr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnfToplevel {
    FunDef(Sym, Vec<(Sym, Type)>, AnfExpr, Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnfProg(pub Vec<AnfToplevel>);

impl Deref for AnfProg {
    type Target = Vec<AnfToplevel>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
