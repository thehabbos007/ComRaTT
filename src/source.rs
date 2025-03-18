use ena::unify::{EqUnifyValue, UnifyKey};
use std::{collections::HashSet, ops::Deref};

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Mul,
    Div,
    Sub,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Const {
    CInt(i32),
    CBool(bool),
    CUnit,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    Const(Const),
    Var(String),
    Lam(Vec<String>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Prim(Binop, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    Delay(Box<Expr>, HashSet<String>),
    Advance(String),
    Tuple(Vec<Expr>),
    Access(Box<Expr>, i32),
}

impl Expr {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Debug)]
pub struct TypeVar(pub u32);
impl UnifyKey for TypeVar {
    type Value = Option<Type>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "TypeVar"
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord)]
pub enum Type {
    TInt,
    TBool,
    TUnit,
    TFun(Box<Type>, Box<Type>),
    TProduct(Vec<Type>),
    TVar(TypeVar),
}

impl EqUnifyValue for Type {}

impl Type {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), Type> {
        match self {
            Type::TInt => Ok(()),
            Type::TBool => Ok(()),
            Type::TUnit => Ok(()),
            Type::TVar(v) => {
                if *v == var {
                    Err(Type::TVar(*v))
                } else {
                    Ok(())
                }
            }
            Type::TFun(from, to) => {
                from.occurs_check(var).map_err(|_| self.clone())?;
                to.occurs_check(var).map_err(|_| self.clone())
            }
            Type::TProduct(ts) => {
                for t in ts {
                    t.occurs_check(var).map_err(|_| self.clone())?
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Toplevel {
    /// A top level function defintion. The type provided is a type for the entire expression,
    /// including args and return type.
    FunDef(String, Type, Vec<String>, Expr),
    Channel(String),
    Output(String, Expr),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Prog(pub Vec<Toplevel>);

impl Deref for Prog {
    type Target = Vec<Toplevel>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Vec<Toplevel>> for Prog {
    fn from(v: Vec<Toplevel>) -> Self {
        Prog(v)
    }
}
