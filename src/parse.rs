use std::ops::Range;

use winnow::ascii::dec_int;
use winnow::combinator::{eof, opt};
use winnow::error::{ContextError, ParseError};
use winnow::prelude::*;
use winnow::{
    ascii::{alpha1, digit1},
    combinator::{alt, delimited, preceded, repeat, separated, terminated},
    stream::AsChar,
    token::{one_of, take_while},
    ModalResult, Parser,
};

use crate::error::ComRaTTError;
use crate::source::{Binop, Const, Expr, Prog, Toplevel, Type};

// Token parsers
fn ident<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanumeric() || c == '_'),
    )
        .take()
        .parse_next(input)
}

fn integer<'s>(input: &mut &'s str) -> ModalResult<i32> {
    dec_int(input)
}

// Type parsers
fn atomic_typ<'s>(input: &mut &'s str) -> ModalResult<Type> {
    alt((
        "int".value(Type::TInt),
        "bool".value(Type::TBool),
        "unit".value(Type::TUnit),
        delimited('(', typ, ')'),
    ))
    .parse_next(input)
}

fn arrow_typ<'s>(input: &mut &'s str) -> ModalResult<Type> {
    let t1 = atomic_typ.parse_next(input)?;
    let t2 = opt(preceded("->", arrow_typ)).parse_next(input)?;

    Ok(match t2 {
        Some(t2) => Type::TFun(t1.b(), t2.b()),
        None => t1,
    })
}

fn typ<'s>(input: &mut &'s str) -> ModalResult<Type> {
    arrow_typ.parse_next(input)
}

// Expression parsers
fn simple_expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    alt((
        ident.map(|s| Expr::Var(s.to_string())),
        integer.map(|i| Expr::Const(Const::CInt(i))),
        "true".value(Expr::Const(Const::CBool(true))),
        "false".value(Expr::Const(Const::CBool(false))),
        "unit".value(Expr::Const(Const::CUnit)),
        delimited('(', expr, ')'),
    ))
    .parse_next(input)
}

fn binop<'s>(input: &mut &'s str) -> ModalResult<Binop> {
    alt((
        "+".value(Binop::Add),
        "*".value(Binop::Mul),
        "/".value(Binop::Div),
        "-".value(Binop::Sub),
        "==".value(Binop::Eq),
        "<".value(Binop::Lt),
        "<=".value(Binop::Lte),
        ">".value(Binop::Gt),
        ">=".value(Binop::Gte),
        "!=".value(Binop::Neq),
    ))
    .parse_next(input)
}

fn arith_expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    let first = simple_expr.parse_next(input)?;

    repeat(0.., (binop, simple_expr))
        .fold(
            move || first.clone(),
            |acc, (op, expr)| Expr::Prim(op, acc.b(), expr.b()),
        )
        .parse_next(input)
}

fn app_expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    alt((
        preceded("delay", simple_expr).map(|e| Expr::Delay(Box::new(e))),
        preceded("advance", ident).map(|s| Expr::Advance(s.to_string())),
        separated(1.., simple_expr, ' ').map(|mut exprs: Vec<Expr>| {
            let mut result = exprs.remove(0);
            for expr in exprs {
                result = Expr::App(result.b(), expr.b());
            }
            result
        }),
        arith_expr,
    ))
    .parse_next(input)
}

fn expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    alt((
        preceded("let", (ident, preceded("=", expr), preceded("in", expr)))
            .map(|(x, e1, e2)| Expr::Let(x.to_string(), e1.b(), e2.b())),
        preceded(
            "lambda",
            (
                repeat(0.., ident)
                    .map(|xs: Vec<&str>| xs.into_iter().map(|x| x.to_string()).collect()),
                preceded("->", expr),
            ),
        )
        .map(|(xs, e)| Expr::Lam(xs, e.b())),
        preceded(
            "if",
            (
                app_expr,
                preceded("then", app_expr),
                preceded("else", app_expr),
            ),
        )
        .map(|(guard, then_branch, else_branch)| {
            Expr::IfThenElse(
                Box::new(guard),
                Box::new(then_branch),
                Box::new(else_branch),
            )
        }),
        app_expr,
    ))
    .parse_next(input)
}

// Top-level parsers
fn fundef<'s>(input: &mut &'s str) -> ModalResult<Toplevel> {
    let mut parser = (
        ident,
        preceded(":", typ),
        preceded("let", ident),
        repeat(0.., ident).map(|xs: Vec<&str>| xs.into_iter().map(|x| x.to_string()).collect()),
        preceded("=", expr),
        ";",
    )
        .map(|(name, t, _def_name, args, e, _)| {
            Toplevel::FunDef(name.to_string(), Box::new(t), args, Box::new(e))
        });

    parser.parse_next(input)
}

pub fn prog<'s>(input: &mut &'s str) -> ModalResult<Prog> {
    terminated(repeat(0.., fundef).map(|tops| Prog(tops)), eof).parse_next(input)
}

impl std::str::FromStr for Prog {
    type Err = ComRaTTError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        prog.parse(input)
            .map_err(|e| ComRaTTError::from_parse(e, input))
    }
}
