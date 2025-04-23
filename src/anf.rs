use crate::source::{Binop, Const, Type};
use crate::types::Sym;
use std::collections::BTreeSet;
use std::ops::Deref;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AExpr {
    Const(Const, Type),
    Var(Sym, Type),
    Lam(Vec<(Sym, Type)>, Box<AnfExpr>, Type),
    Wait(Sym, Type),
}

impl AExpr {
    pub fn ty(&self) -> Type {
        match self {
            AExpr::Const(_, ty) => ty.clone(),
            AExpr::Var(_, ty) => ty.clone(),
            AExpr::Lam(.., ty) => ty.clone(),
            AExpr::Wait(_, ty) => ty.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CExpr {
    Prim(Binop, AExpr, AExpr, Type),
    App(AExpr, Vec<AExpr>, Type),
    Tuple(Vec<AExpr>, Type),
    Access(AExpr, i32, Type),
    Sig(AExpr, AExpr, Type),
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
            CExpr::Sig(_, _, ty) => ty.clone(),
        }
    }

    pub fn traverse_locals<'a>(&'a self, locals: &mut BTreeSet<(&'a str, Type)>) {
        if let CExpr::IfThenElse(_, then_branch, else_branch, _) = self {
            then_branch.traverse_locals(locals);
            else_branch.traverse_locals(locals);
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
            AnfExpr::Let(name, _, rhs, body) => {
                locals.insert((name, rhs.ty()));
                rhs.traverse_locals(locals);
                body.traverse_locals(locals);
            }
            AnfExpr::CExp(c) => {
                c.traverse_locals(locals);
            }
            _ => {}
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            AnfExpr::AExpr(expr) => expr.ty(),
            AnfExpr::CExp(expr) => expr.ty(),
            AnfExpr::Let(_, _, _, body) => body.ty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnfToplevel {
    FunDef(Sym, Vec<(Sym, Type)>, AnfExpr, Type),
    Channel(String, Type),
    Output(String, AExpr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnfProg(pub Vec<AnfToplevel>, pub Vec<(Sym, Type)>);

impl Deref for AnfProg {
    type Target = Vec<AnfToplevel>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_traverse_locals() {
        let mut locals = BTreeSet::new();

        // Test let binding
        let expr = AnfExpr::Let(
            "x".into(),
            Type::TInt,
            Box::new(AnfExpr::AExpr(AExpr::Const(Const::CInt(1), Type::TInt))),
            Box::new(AnfExpr::AExpr(AExpr::Var("x".into(), Type::TInt))),
        );

        expr.traverse_locals(&mut locals);
        assert_eq!(locals.len(), 1);
        assert!(locals.contains(&("x", Type::TInt)));

        locals.clear();

        // Test nested let bindings
        let expr = AnfExpr::Let(
            "x".into(),
            Type::TInt,
            Box::new(AnfExpr::Let(
                "y".into(),
                Type::TBool,
                Box::new(AnfExpr::AExpr(AExpr::Const(
                    Const::CBool(true),
                    Type::TBool,
                ))),
                Box::new(AnfExpr::AExpr(AExpr::Var("y".into(), Type::TBool))),
            )),
            Box::new(AnfExpr::AExpr(AExpr::Var("x".into(), Type::TInt))),
        );

        expr.traverse_locals(&mut locals);
        assert_eq!(locals.len(), 2);
        assert!(locals.contains(&("x", Type::TInt)));
        assert!(locals.contains(&("y", Type::TBool)));

        locals.clear();

        // Test if-then-else
        let expr = AnfExpr::CExp(CExpr::IfThenElse(
            AExpr::Const(Const::CBool(true), Type::TBool),
            Box::new(AnfExpr::Let(
                "x".into(),
                Type::TInt,
                Box::new(AnfExpr::AExpr(AExpr::Const(Const::CInt(1), Type::TInt))),
                Box::new(AnfExpr::AExpr(AExpr::Var("x".into(), Type::TInt))),
            )),
            Box::new(AnfExpr::Let(
                "y".into(),
                Type::TInt,
                Box::new(AnfExpr::AExpr(AExpr::Const(Const::CInt(2), Type::TInt))),
                Box::new(AnfExpr::AExpr(AExpr::Var("y".into(), Type::TInt))),
            )),
            Type::TInt,
        ));

        expr.traverse_locals(&mut locals);
        assert_eq!(locals.len(), 2);
        assert!(locals.contains(&("x", Type::TInt)));
        assert!(locals.contains(&("y", Type::TInt)));
    }
}
