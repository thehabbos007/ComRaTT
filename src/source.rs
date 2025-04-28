use ena::unify::{EqUnifyValue, UnifyKey};
use std::{collections::BTreeSet, ops::Deref};

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

pub type ClockExprs = BTreeSet<ClockExpr>;
pub fn empty_clock_expr() -> ClockExprs {
    BTreeSet::new()
}

impl From<ClockExpr> for ClockExprs {
    fn from(v: ClockExpr) -> Self {
        let mut set = BTreeSet::new();
        set.insert(v);
        set
    }
}

/// Clock Expr. 𝜃 ::= cl (𝑣) | 𝜃 ⊔ 𝜃 ′
#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum ClockExpr {
    /// Symbolic clock that is evaluated at runtime
    Symbolic,
    /// Cl(v) where v is a binding
    Cl(String),
    /// Cl(v) special case, where v is the name of a channel
    Wait(String),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Const {
    CInt(i32),
    CBool(bool),
    CUnit,
    CLaterUnit,
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
    Delay(Box<Expr>, ClockExprs),
    Advance(String),
    Wait(String),
    Tuple(Vec<Expr>),
    Sig(Box<Expr>, Box<Expr>),
    Access(Box<Expr>, i32),
    Box(Box<Expr>),
    Unbox(Box<Expr>),
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
    TLaterUnit(ClockExprs),
    TFun(Box<Type>, Box<Type>),
    TProduct(Vec<Type>),
    TSig(Box<Type>),
    TVar(TypeVar),
    TBox(Box<Type>),
}

impl EqUnifyValue for Type {}

impl Type {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn contains_later_unit(&self) -> bool {
        matches!(self, Type::TFun(box Type::TLaterUnit(_), _) | Type::TLaterUnit(_))
    }

    pub fn occurs_check(&self, var: TypeVar) -> Result<(), Type> {
        match self {
            Type::TInt => Ok(()),
            Type::TBool => Ok(()),
            Type::TLaterUnit(_) => Ok(()),
            Type::TUnit => Ok(()),
            Type::TSig(_) => Ok(()),
            Type::TBox(_) => Ok(()),
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
    Channel(String, Type),
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
