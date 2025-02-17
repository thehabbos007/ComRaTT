use std::fmt::Display;

use itertools::Itertools;

use crate::{
    source::*,
    types::{TypedExpr, TypedProg, TypedToplevel},
};

impl TypedExpr {
    pub fn untyped(&self) -> Expr {
        match self {
            TypedExpr::TConst(c, _) => Expr::Const(c.clone()),
            TypedExpr::TName(x, _) => Expr::Var(x.clone()),
            TypedExpr::TLam(args, body, _) => Expr::Lam(
                args.clone().into_iter().map(|(name, _)| name).collect_vec(),
                body.untyped().b(),
            ),
            TypedExpr::TApp(e1, args, _) => args.iter().fold(e1.untyped(), |acc, arg| {
                Expr::App(acc.b(), arg.untyped().b())
            }),
            TypedExpr::TPrim(op, e1, e2, _) => Expr::Prim(*op, e1.untyped().b(), e2.untyped().b()),
            TypedExpr::TLet(x, _, e1, e2) => {
                Expr::Let(x.clone(), e1.untyped().b(), e2.untyped().b())
            }
            TypedExpr::TIfThenElse(cond, then_branch, else_branch, _) => Expr::IfThenElse(
                cond.untyped().b(),
                then_branch.untyped().b(),
                else_branch.untyped().b(),
            ),
            TypedExpr::TTuple(exprs, _) => {
                Expr::Tuple(exprs.iter().map(|e| e.untyped()).collect_vec())
            }
            TypedExpr::TAccess(e, i, _) => Expr::Access(e.untyped().b(), *i),
        }
    }

    pub fn to_string(&self) -> String {
        self.untyped().to_string()
    }
}

impl TypedToplevel {
    pub fn untyped(&self) -> Toplevel {
        match self {
            TypedToplevel::TFunDef(name, args, body, typ) => Toplevel::FunDef(
                name.clone(),
                typ.clone(),
                args.clone().into_iter().map(|(name, _)| name).collect_vec(),
                body.untyped().b(),
            ),
        }
    }

    pub fn to_string(&self) -> String {
        self.untyped().to_string()
    }
}

impl TypedProg {
    pub fn untyped(&self) -> Prog {
        Prog(
            self.0
                .iter()
                .map(|def| match def {
                    TypedToplevel::TFunDef(name, args, body, typ) => Toplevel::FunDef(
                        name.clone(),
                        typ.clone(),
                        args.clone().into_iter().map(|(name, _)| name).collect_vec(),
                        body.untyped().b(),
                    ),
                })
                .collect(),
        )
    }

    pub fn to_string(&self) -> String {
        self.untyped().to_string()
    }
}

impl Binop {
    fn to_string(&self) -> String {
        match self {
            Binop::Add => "+".to_string(),
            Binop::Mul => "*".to_string(),
            Binop::Div => "/".to_string(),
            Binop::Sub => "-".to_string(),
            Binop::Eq => "=".to_string(),
            Binop::Neq => "<>".to_string(),
            Binop::Lt => "<".to_string(),
            Binop::Lte => "<=".to_string(),
            Binop::Gt => ">".to_string(),
            Binop::Gte => ">=".to_string(),
        }
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Const {
    fn to_string(&self) -> String {
        match self {
            Const::CInt(n) => n.to_string(),
            Const::CBool(true) => "true".to_string(),
            Const::CBool(false) => "false".to_string(),
            Const::CUnit => "()".to_string(),
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Type {
    fn to_string(&self) -> String {
        match self {
            Type::TInt => "int".to_string(),
            Type::TBool => "bool".to_string(),
            Type::TUnit => "()".to_string(),
            Type::TFun(t1, t2) => {
                let t1_str = match **t1 {
                    Type::TFun(_, _) => format!("({})", t1.to_string()),
                    _ => t1.to_string(),
                };
                format!("{} -> {}", t1_str, t2.to_string())
            }
            Type::TProduct(types) => {
                format!(
                    "({})",
                    types
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" * ")
                )
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Const(c) => c.to_string(),
            Expr::Var(x) => x.clone(),
            Expr::Lam(args, body) => {
                format!("fun {} -> {}", args.join(" "), body.to_string())
            }
            Expr::App(e1, e2) => {
                let e1_str = match **e1 {
                    Expr::Lam(_, _) => format!("({})", e1.to_string()),
                    _ => e1.to_string(),
                };
                let e2_str = match **e2 {
                    Expr::App(_, _) | Expr::Lam(_, _) | Expr::Prim(_, _, _) => {
                        format!("({})", e2.to_string())
                    }
                    _ => e2.to_string(),
                };
                format!("{} {}", e1_str, e2_str)
            }
            Expr::Prim(op, e1, e2) => {
                let e1_str = match **e1 {
                    Expr::Prim(_, _, _) => format!("({})", e1.to_string()),
                    _ => e1.to_string(),
                };
                let e2_str = match **e2 {
                    Expr::Prim(_, _, _) => format!("({})", e2.to_string()),
                    _ => e2.to_string(),
                };
                format!("{} {} {}", e1_str, op.to_string(), e2_str)
            }
            Expr::Let(x, e1, e2) => {
                format!("let {} = {} in {}", x, e1.to_string(), e2.to_string())
            }
            Expr::IfThenElse(cond, then_branch, else_branch) => {
                format!(
                    "if {} then {} else {}",
                    cond.to_string(),
                    then_branch.to_string(),
                    else_branch.to_string()
                )
            }
            Expr::Delay(e) => format!("delay {}", e.to_string()),
            Expr::Advance(x) => format!("advance {}", x),
            Expr::Tuple(exprs) => {
                format!(
                    "({})",
                    exprs
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expr::Access(e, i) => format!("{}.{}", e.to_string(), i),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Toplevel {
    fn to_string(&self) -> String {
        match self {
            Toplevel::FunDef(name, typ, args, body) => {
                format!(
                    "{}: {}\nlet {} {} = {};\n",
                    name,
                    typ.to_string(),
                    name,
                    args.join(" "),
                    body.to_string()
                )
            }
        }
    }
}

impl Display for Toplevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Prog {
    pub fn to_string(&self) -> String {
        self.0.iter().map(|def| def.to_string()).collect()
    }
}

impl Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
