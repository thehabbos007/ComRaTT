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
            TypedExpr::TConst(c, _) => Expr::Const(c.clone()),
            TypedExpr::TName(x, _) => Expr::Var(x.clone()),
            TypedExpr::TLam(args, body, _, _) => Expr::Lam(
                args.clone().into_iter().map(|(name, _)| name).collect_vec(),
                body.untyped().b(),
            ),
            TypedExpr::TApp(e1, args, _) => args.iter().fold(e1.untyped(), |acc, arg| {
                Expr::App(acc.b(), vec![arg.untyped()])
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
            TypedExpr::TWait(name, _) => Expr::Wait(name.clone()),
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
            TypedToplevel::Channel(name, t) => Toplevel::Channel(name.to_owned(), t.clone()),
            TypedToplevel::Output(name, typed_expr) => {
                Toplevel::Output(name.to_owned(), typed_expr.untyped())
            }
        }
    }
}

impl TypedProg {
    pub fn untyped(&self) -> Prog {
        Prog(self.defs.iter().map(TypedToplevel::untyped).collect())
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
            Const::CLaterUnit => write!(f, "{{}}"),
            Const::Never => write!(f, "never"),
        }
    }
}

impl Display for ClockExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClockExpr::Symbolic => write!(f, "SYMBOLIC"),
            ClockExpr::Cl(v) => write!(f, "cl({v})"),
            ClockExpr::Wait(v) => write!(f, "cl(wait_{v})"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::TInt => write!(f, "int"),
            Type::TBool => write!(f, "bool"),
            Type::TUnit => write!(f, "()"),
            Type::TLater(ty, clock) => write!(f, "⨀{{{:?}}} {}", clock, ty),
            Type::TVar(type_var) => write!(f, "TVar {}", type_var.0),
            Type::TFun(t1, t2) => write!(f, "{} -> {}", t1, t2),
            Type::TSig(t) => write!(f, "Sig {}", t),
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
            Type::TBox(ty) => write!(f, "□ ({ty})"),
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
            Expr::App(e1, es) => {
                let e1_str = match **e1 {
                    Expr::Lam(_, _) => format!("({})", e1),
                    _ => e1.to_string(),
                };
                format!("{}({})", e1_str, es.iter().join(", "))
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
            Expr::Delay(e, c) => format!("delay {{{:?}}} {}", c, e),
            Expr::Advance(x) => format!("advance {}", x),
            Expr::Sig(e1, e2) => {
                format!("{} :: {}", e1, e2)
            }
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
            Expr::Wait(i) => format!("wait {}", i),
            Expr::Box(e) => format!("box {}", e),
            Expr::Unbox(e) => format!("unbox {}", e),
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
            Toplevel::Channel(channel, t) => format!("chan {channel}: {t};"),
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

impl Display for TypedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedExpr::TConst(c, _) => write!(f, "{}", c),
            TypedExpr::TName(x, _) => write!(f, "{}", x),
            TypedExpr::TLam(args, body, _ty, clock) => {
                let args_str = args
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>()
                    .join(" ");
                // write!(
                //     f,
                //     "(fun{{{:?}}} [{}] -> {} : {})",
                //     clock, args_str, body, ty
                // )
                write!(f, "(fun{{{:?}}} [{}] -> {})", clock, args_str, body)
            }
            TypedExpr::TApp(e, args, _ty) => {
                let args_str = args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                // write!(f, "{} ({}) : {}", e, args_str, ty)
                write!(f, "{} ({})", e, args_str)
            }
            TypedExpr::TPrim(op, e1, e2, _) => write!(f, "{} {} {}", e1, op, e2),
            TypedExpr::TLet(x, _, e1, e2) => {
                write!(f, "let {} = {} in {}", x, e1, e2)
            }
            TypedExpr::TIfThenElse(cond, then_branch, else_branch, _) => {
                write!(f, "if {} then {} else {}", cond, then_branch, else_branch)
            }
            TypedExpr::TTuple(exprs, _) => {
                let exprs_str = exprs
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({})", exprs_str)
            }
            TypedExpr::TAccess(e, i, _) => write!(f, "{}.{}", e, i),
            TypedExpr::TWait(name, _) => write!(f, "wait {}", name),
        }
    }
}

impl Display for TypedToplevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedToplevel::TFunDef(name, params, body, ret_ty) => {
                let param_str = params
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(f, "def {}({}) : {} =\n  {}", name, param_str, ret_ty, body)
            }
            TypedToplevel::Channel(name, ty) => writeln!(f, "channel {} : {};", name, ty),
            TypedToplevel::Output(name, expr) => writeln!(f, "{} <- {};", name, expr),
        }
    }
}

impl Display for TypedProg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in self.defs.iter() {
            writeln!(f, "{}", p)?;
        }

        writeln!(f)
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
                    .join(", ");
                writeln!(f, "def {}({}) : {} =\n  {}", name, param_str, ret_ty, body)
            }
            AnfToplevel::Channel(name, typ) => writeln!(f, "channel {} : {};", name, typ),
            AnfToplevel::Output(name, aexpr) => writeln!(f, "{} <- {};", name, aexpr),
        }
    }
}

impl Display for AnfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnfExpr::AExpr(a) => write!(f, "{}", a),
            AnfExpr::CExp(c) => write!(f, "{}", c),
            AnfExpr::Let(name, _, rhs, body) => {
                write!(f, "let {} = {} in {}", name, rhs, body)
            }
        }
    }
}

impl Display for AExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AExpr::Const(c, _) => write!(f, "{}", c),
            // AExpr::Var(s, t) => write!(f, "{}: {}", s, t),
            AExpr::Var(s, _) => write!(f, "{}", s),
            AExpr::LaterClosure(body, clock, _) => {
                write!(f, "(⨂{{{:?}}} -> {})", clock, body)
            }
            AExpr::Closure(args, body, _) => {
                let args_str = args
                    .iter()
                    .map(|ty| (&ty).to_string())
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(f, "(Clos {} -> {})", args_str, body)
            }
            AExpr::Wait(name, _) => write!(f, "wait_ffi {}", &name),
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
                write!(f, "{} ({})", fun, args_str)
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
