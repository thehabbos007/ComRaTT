use crate::error::ComRaTTError;

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
    Delay(Box<Expr>),
    Advance(String),
    Tuple(Vec<Expr>),
    Access(Box<Expr>, i32),
}

impl Expr {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    TInt,
    TBool,
    TUnit,
    TFun(Box<Type>, Box<Type>),
    TProduct(Vec<Type>),
}

impl Type {
    pub fn b(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Toplevel {
    FunDef(String, Box<Type>, Vec<String>, Box<Expr>),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Prog(pub Vec<Toplevel>);

// impl Prog {
//     pub fn parse(input: &str) -> Result<Prog, ComRaTTError> {
//         input.parse::<Prog>()
//     }
// }
