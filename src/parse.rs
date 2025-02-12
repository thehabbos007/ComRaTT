use winnow::ascii::{dec_int, multispace0};
use winnow::combinator::{eof, opt, trace};
use winnow::error::{ParserError, StrContext, StrContextValue};
use winnow::{
    combinator::{alt, delimited, preceded, repeat, separated, terminated},
    stream::AsChar,
    token::{one_of, take_while},
    ModalResult, Parser,
};

use crate::error::ComRaTTError;
use crate::source::{Binop, Const, Expr, Prog, Toplevel, Type};

fn ws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn wsr<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    terminated(inner, multispace0)
}

fn lws<'a, F, O, E: ParserError<&'a str>>(inner: F) -> impl Parser<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    preceded(multispace0, inner)
}

// Token parsers
fn ident<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
    (
        one_of(|c: char| c.is_alpha() || c == '_'),
        take_while(0.., |c: char| c.is_alphanum() || c == '_'),
    )
        .take()
        .context(StrContext::Label("identifier"))
        .context(StrContext::Expected(StrContextValue::Description(
            "valid identifier",
        )))
        .parse_next(input)
}

fn integer<'s>(input: &mut &'s str) -> ModalResult<i32> {
    dec_int
        .context(StrContext::Label("integer"))
        .context(StrContext::Expected(StrContextValue::Description(
            "decimal integer",
        )))
        .parse_next(input)
}

// Type parsers
fn atomic_typ<'s>(input: &mut &'s str) -> ModalResult<Type> {
    alt((
        ws("bool").value(Type::TBool),
        ws("int").value(Type::TInt),
        ws("unit").value(Type::TUnit),
        delimited('(', typ, ')'),
    ))
    .context(StrContext::Label("type"))
    .context(StrContext::Expected(StrContextValue::Description(
        "atomic type",
    )))
    .parse_next(input)
}

fn arrow_typ<'s>(input: &mut &'s str) -> ModalResult<Type> {
    let t1 = atomic_typ.parse_next(input)?;
    let t2 = opt(preceded(ws("->"), arrow_typ)).parse_next(input)?;

    Ok(match t2 {
        Some(t2) => Type::TFun(t1.b(), t2.b()),
        None => t1,
    })
}

fn typ<'s>(input: &mut &'s str) -> ModalResult<Type> {
    arrow_typ
        .context(StrContext::Label("type"))
        .context(StrContext::Expected(StrContextValue::Description(
            "type expression",
        )))
        .parse_next(input)
}

// Expression parsers
fn simple_expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    alt((
        ws("true").value(Expr::Const(Const::CBool(true))),
        ws("false").value(Expr::Const(Const::CBool(false))),
        ws("()").value(Expr::Const(Const::CUnit)),
        ws(ident).map(|s| Expr::Var(s.to_string())),
        ws(integer).map(|i| Expr::Const(Const::CInt(i))),
        delimited('(', expr, ')'),
    ))
    .context(StrContext::Label("simple expression"))
    .context(StrContext::Expected(StrContextValue::Description(
        "simple expression",
    )))
    .parse_next(input)
}

fn binop<'s>(input: &mut &'s str) -> ModalResult<Binop> {
    alt((
        ws("+").value(Binop::Add),
        ws("*").value(Binop::Mul),
        ws("/").value(Binop::Div),
        ws("-").value(Binop::Sub),
        ws("==").value(Binop::Eq),
        ws("<").value(Binop::Lt),
        ws("<=").value(Binop::Lte),
        ws(">").value(Binop::Gt),
        ws(">=").value(Binop::Gte),
        ws("!=").value(Binop::Neq),
    ))
    .context(StrContext::Label("binary operator"))
    .context(StrContext::Expected(StrContextValue::Description(
        "binary operator",
    )))
    .parse_next(input)
}

fn arith_expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    let first = simple_expr.parse_next(input)?;

    repeat(0.., (binop, simple_expr))
        .fold(
            move || first.clone(),
            |acc, (op, expr)| Expr::Prim(op, acc.b(), expr.b()),
        )
        .context(StrContext::Label("arithmetic expression"))
        .context(StrContext::Expected(StrContextValue::Description(
            "arithmetic expression",
        )))
        .parse_next(input)
}

fn app_expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    alt((
        preceded(ws("delay"), simple_expr).map(|e| Expr::Delay(Box::new(e))),
        preceded(ws("advance"), ws(ident)).map(|s| Expr::Advance(s.to_string())),
        separated(1.., simple_expr, ws(' ')).map(|mut exprs: Vec<Expr>| {
            let mut result = exprs.remove(0);
            for expr in exprs {
                result = Expr::App(result.b(), expr.b());
            }
            result
        }),
        arith_expr,
    ))
    .context(StrContext::Label("application expression"))
    .context(StrContext::Expected(StrContextValue::Description(
        "application expression",
    )))
    .parse_next(input)
}

fn expr<'s>(input: &mut &'s str) -> ModalResult<Expr> {
    alt((
        preceded(
            ws("let"),
            (ws(ident), preceded(ws("="), expr), preceded(ws("in"), expr)),
        )
        .map(|(x, e1, e2)| Expr::Let(x.to_string(), e1.b(), e2.b())),
        preceded(
            ws("lambda"),
            (
                repeat(0.., ws(ident))
                    .map(|xs: Vec<&str>| xs.into_iter().map(|x| x.to_string()).collect()),
                preceded(ws("->"), expr),
            ),
        )
        .map(|(xs, e)| Expr::Lam(xs, e.b())),
        preceded(
            ws("if"),
            (
                app_expr,
                preceded(ws("then"), app_expr),
                preceded(ws("else"), app_expr),
            ),
        )
        .map(|(guard, then_branch, else_branch)| {
            Expr::IfThenElse(
                Box::new(guard),
                Box::new(then_branch),
                Box::new(else_branch),
            )
        }),
        arith_expr,
        app_expr,
    ))
    .context(StrContext::Label("expression"))
    .context(StrContext::Expected(StrContextValue::Description(
        "expression",
    )))
    .parse_next(input)
}

// Top-level parsers
fn fundef<'s>(input: &mut &'s str) -> ModalResult<Toplevel> {
    let mut parser = (
        wsr(ident).context(StrContext::Expected(StrContextValue::Description(
            "function name",
        ))),
        preceded(ws(":"), wsr(typ)).context(StrContext::Expected(StrContextValue::Description(
            "function type annotation",
        ))),
        preceded(ws("let"), wsr(ident)).context(StrContext::Expected(
            StrContextValue::Description("binding name"),
        )),
        repeat(0.., ws(ident))
            .map(|xs: Vec<&str>| xs.into_iter().map(|x| x.to_string()).collect())
            .context(StrContext::Expected(StrContextValue::Description(
                "function parameters",
            ))),
        preceded(ws("="), expr).context(StrContext::Expected(StrContextValue::Description(
            "function body",
        ))),
        lws(";"),
    )
        .map(|(name, t, _def_name, args, e, _)| {
            Toplevel::FunDef(name.to_string(), t.b(), args, e.b())
        })
        .context(StrContext::Label("function definition"))
        .context(StrContext::Expected(StrContextValue::Description(
            "function definition",
        )));

    parser.parse_next(input)
}

pub fn prog<'s>(input: &mut &'s str) -> ModalResult<Prog> {
    terminated(repeat(0.., ws(fundef)).map(Prog), eof)
        .context(StrContext::Label("program"))
        .context(StrContext::Expected(StrContextValue::Description(
            "top level function definition",
        )))
        .parse_next(input)
}

impl std::str::FromStr for Prog {
    type Err = ComRaTTError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        prog.parse(input).map_err(|e| {
            eprintln!("{e:?}");
            ComRaTTError::from_parse(e, input)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_idents() {
        assert_eq!(ident.parse("x"), Ok("x"));
        assert_eq!(ident.parse("foo"), Ok("foo"));
        assert_eq!(ident.parse("bar123"), Ok("bar123"));
        assert_eq!(ident.parse("foo_bar"), Ok("bar123"));
        assert_eq!(ident.parse("_test"), Ok("_test"));
        assert!(ident.parse("123abc").is_err());
        assert!(ident.parse("").is_err());
    }

    #[test]
    fn test_atomic_types() {
        assert_eq!(typ.parse("int"), Ok(Type::TInt));
        assert_eq!(typ.parse("bool"), Ok(Type::TBool));
        assert_eq!(typ.parse("unit"), Ok(Type::TUnit));
        assert_eq!(typ.parse("(int)"), Ok(Type::TInt));
    }

    #[test]
    fn test_arrow_types() {
        assert_eq!(
            typ.parse("int -> bool"),
            Ok(Type::TFun(Box::new(Type::TInt), Box::new(Type::TBool)))
        );
        assert_eq!(
            typ.parse("int -> bool -> unit"),
            Ok(Type::TFun(
                Box::new(Type::TInt),
                Box::new(Type::TFun(Box::new(Type::TBool), Box::new(Type::TUnit)))
            ))
        );
    }

    #[test]
    fn test_simple_exprs() {
        assert_eq!(expr.parse("x"), Ok(Expr::Var("x".to_string())));
        assert_eq!(expr.parse("42"), Ok(Expr::Const(Const::CInt(42))));
        assert_eq!(expr.parse("true"), Ok(Expr::Const(Const::CBool(true))));
        assert_eq!(expr.parse("false"), Ok(Expr::Const(Const::CBool(false))));
        assert_eq!(expr.parse("()"), Ok(Expr::Const(Const::CUnit)));
    }

    #[test]
    fn test_binop_exprs() {
        assert_eq!(
            expr.parse("1 + 2"),
            Ok(Expr::Prim(
                Binop::Add,
                Box::new(Expr::Const(Const::CInt(1))),
                Box::new(Expr::Const(Const::CInt(2)))
            ))
        );
    }

    #[test]
    fn test_let_expr() {
        assert_eq!(
            expr.parse("let x = 1 in x"),
            Ok(Expr::Let(
                "x".to_string(),
                Box::new(Expr::Const(Const::CInt(1))),
                Box::new(Expr::Var("x".to_string()))
            ))
        );
    }

    #[test]
    fn test_lambda_expr() {
        assert_eq!(
            expr.parse("lambda x -> x"),
            Ok(Expr::Lam(
                vec!["x".to_string()],
                Box::new(Expr::Var("x".to_string()))
            ))
        );
    }

    #[test]
    fn test_if_expr() {
        assert_eq!(
            expr.parse("if true then 1 else 0"),
            Ok(Expr::IfThenElse(
                Box::new(Expr::Const(Const::CBool(true))),
                Box::new(Expr::Const(Const::CInt(1))),
                Box::new(Expr::Const(Const::CInt(0)))
            ))
        );
    }

    #[test]
    fn test_function_def() {
        let input = "id: int -> int let id x = x;";
        assert_eq!(
            fundef.parse(input),
            Ok(Toplevel::FunDef(
                "id".to_string(),
                Box::new(Type::TFun(Box::new(Type::TInt), Box::new(Type::TInt))),
                vec!["x".to_string()],
                Box::new(Expr::Var("x".to_string()))
            ))
        );
    }

    #[test]
    fn test_program_many_def() {
        let input = r#"
          id: int -> int
          let id x = x;

          const: int
          let const y = 42;
        "#;
        let result = prog.parse(input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().0.len(), 2);
    }

    #[test]
    fn test_is_prime() {
        let input = r#"
            mod_op : int -> int -> int
            let mod_op n m =
              n - ((n / m) * m);

            check_prime_helper : int -> int -> bool
            let check_prime_helper n divisor =3;
        "#;

        let result = prog.parse(input);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().0.len(), 2);
    }
}
