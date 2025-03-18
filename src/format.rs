use std::fmt::Display;

use itertools::Itertools;

use crate::{
    anf::{AExpr, AnfExpr, AnfProg, AnfToplevel, CExpr},
    source::*,
    types::{TypedExpr, TypedProg, TypedToplevel},
};

impl TypedExpr {
    pub fn untyped(&self) -> Expr {
        match self {
            TypedExpr::TConst(c, _) => Expr::Const(*c),
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
}

impl TypedToplevel {
    pub fn untyped(&self) -> Toplevel {
        match self {
            TypedToplevel::TFunDef(name, args, body, typ) => Toplevel::FunDef(
                name.clone(),
                typ.clone(),
                args.clone().into_iter().map(|(name, _)| name).collect_vec(),
                body.untyped(),
            ),
            TypedToplevel::Channel(name) => Toplevel::Channel(name.to_owned()),
            TypedToplevel::Output(name, typed_expr) => {
                Toplevel::Output(name.to_owned(), typed_expr.untyped())
            }
        }
    }
}

impl TypedProg {
    pub fn untyped(&self) -> Prog {
        Prog(self.0.iter().map(TypedToplevel::untyped).collect())
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Binop::Add => "+",
            Binop::Mul => "*",
            Binop::Div => "/",
            Binop::Sub => "-",
            Binop::Eq => "=",
            Binop::Neq => "<>",
            Binop::Lt => "<",
            Binop::Lte => "<=",
            Binop::Gt => ">",
            Binop::Gte => ">=",
        };
        write!(f, "{}", str)
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::CInt(n) => write!(f, "{}", n),
            Const::CBool(true) => write!(f, "true"),
            Const::CBool(false) => write!(f, "false"),
            Const::CUnit => write!(f, "()"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::TInt => write!(f, "int"),
            Type::TBool => write!(f, "bool"),
            Type::TUnit => write!(f, "()"),
            Type::TVar(type_var) => write!(f, "TVar {}", type_var.0),
            Type::TFun(t1, t2) => write!(f, "{} -> {}", t1, t2),
            Type::TProduct(types) => {
                write!(
                    f,
                    "({})",
                    (types
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(" * "))
                )
            }
        }
    }
}

impl Expr {
    fn print_to_str(&self) -> String {
        match self {
            Expr::Const(c) => c.to_string(),
            Expr::Var(x) => x.clone(),
            Expr::Lam(args, body) => {
                format!("fun {} -> {}", args.join(" "), body)
            }
            Expr::App(e1, e2) => {
                let e1_str = match **e1 {
                    Expr::Lam(_, _) => format!("({})", e1),
                    _ => e1.to_string(),
                };
                let e2_str = match **e2 {
                    Expr::App(_, _) | Expr::Lam(_, _) | Expr::Prim(_, _, _) => {
                        format!("({})", e2)
                    }
                    _ => e2.to_string(),
                };
                format!("{} {}", e1_str, e2_str)
            }
            Expr::Prim(op, e1, e2) => {
                let e1_str = match **e1 {
                    Expr::Prim(_, _, _) => format!("({})", e1),
                    _ => e1.to_string(),
                };
                let e2_str = match **e2 {
                    Expr::Prim(_, _, _) => format!("({})", e2),
                    _ => e2.to_string(),
                };
                format!("{} {} {}", e1_str, op, e2_str)
            }
            Expr::Let(x, e1, e2) => {
                format!("let {} = {} in {}", x, e1, e2)
            }
            Expr::IfThenElse(cond, then_branch, else_branch) => {
                format!("if {} then {} else {}", cond, then_branch, else_branch)
            }
            Expr::Delay(e, c) => format!("delay {{{}}} {}", c.iter().join(" "), e),
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
            Expr::Access(e, i) => format!("{}.{}", e, i),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_to_str())
    }
}

impl Toplevel {
    fn print_to_str(&self) -> String {
        match self {
            Toplevel::FunDef(name, typ, args, body) => {
                format!(
                    "{}: {}\nlet {} {} = {};\n",
                    name,
                    typ,
                    name,
                    args.join(" "),
                    body
                )
            }
            Toplevel::Channel(channel) => format!("chan {channel};"),
            Toplevel::Output(out, expr) => format!("{} <- {}", out, expr),
        }
    }
}

impl Display for Toplevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print_to_str())
    }
}

impl Display for Prog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for def in self.0.iter() {
            writeln!(f, "{}", def)?;
        }

        Ok(())
    }
}

impl Display for AnfProg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in self.iter() {
            writeln!(f, "{}", p)?;
        }

        writeln!(f)
    }
}
impl Display for AnfToplevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnfToplevel::FunDef(name, params, body, ret_ty) => {
                let param_str = params
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(" ");
                writeln!(f, "def {}({}) : {} =\n  {}", name, param_str, ret_ty, body)
            }
            AnfToplevel::Channel(name) => writeln!(f, "channel {};", name),
            AnfToplevel::Output(name, aexpr) => writeln!(f, "{} <- {};", name, aexpr),
        }
    }
}

impl Display for AnfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnfExpr::AExpr(a) => write!(f, "{}", a),
            AnfExpr::CExp(c) => write!(f, "{}", c),
            AnfExpr::Let(name, ty, val, body) => {
                write!(f, "let {}: {} = {} in {}", name, ty, val, body)
            }
        }
    }
}

impl Display for AExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AExpr::Const(c, _) => write!(f, "{}", c),
            AExpr::Var(s, _) => write!(f, "{}", s),
        }
    }
}

impl Display for CExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CExpr::Prim(op, a1, a2, _) => write!(f, "{} {} {}", a1, op, a2),
            CExpr::App(fun, args, _) => {
                let args_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "{}({})", fun, args_str)
            }
            CExpr::Tuple(elems, _) => {
                let elems_str = elems
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({})", elems_str)
            }
            CExpr::Access(tup, idx, _) => write!(f, "{}.{}", tup, idx),
            CExpr::IfThenElse(cond, then_branch, else_branch, _) => {
                write!(f, "if {} then {} else {}", cond, then_branch, else_branch)
            }
        }
    }
}
