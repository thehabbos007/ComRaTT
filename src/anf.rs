use crate::source::{Binop, Const, Type};
use crate::types::Sym;
use std::collections::BTreeSet;
use std::ops::Deref;

type Param = (Sym, Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AExpr {
    Const(Const, Type),
    Var(Sym, Type),
    // Lambdas are lifted to top-level functions, so we don't need this..
    // Lam(..)
}

impl AExpr {
    pub fn ty(&self) -> Type {
        match self {
            AExpr::Const(_, ty) => ty.clone(),
            AExpr::Var(_, ty) => ty.clone(),
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

    pub fn traverse_locals<'a>(&'a self, locals: &mut BTreeSet<(&'a str, Type)>) {
        match self {
            CExpr::IfThenElse(_, then_branch, else_branch, _) => {
                then_branch.traverse_locals(locals);
                else_branch.traverse_locals(locals);
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnfExpr {
    AExpr(AExpr),
    CExp(CExpr),
    Let(Sym, Type, Box<AnfExpr>, Box<AnfExpr>),
}

impl AnfExpr {
    pub fn traverse_locals<'a>(&'a self, locals: &mut BTreeSet<(&'a str, Type)>) {
        match self {
            AnfExpr::Let(name, ty, val, body) => {
                locals.insert((name, ty.clone()));
                val.traverse_locals(locals);
                body.traverse_locals(locals);
            }
            _ => {}
        }
    }
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
