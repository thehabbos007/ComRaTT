use itertools::Itertools;
use winnow::combinator::opt;
use winnow::error::{ContextError, StrContext, StrContextValue};
use winnow::token::literal;
use winnow::{
    combinator::{alt, delimited, preceded, repeat, terminated},
    Parser, Result,
};

use crate::error::ComRaTTError;
use crate::lexer::{Token, TokenKind, Tokenizer};
use crate::source::{Binop, Const, Expr, Prog, Toplevel, Type};

pub type TokenSlice<'a> = winnow::stream::TokenSlice<'a, Token<'a>>;
pub type Input<'a> = TokenSlice<'a>;

impl Prog {
    pub fn parse(input: &str) -> Result<Prog, ComRaTTError> {
        let tokenizer = Tokenizer::tokenize(input)
            .filter_map(|t| t.ok())
            .collect_vec();

        let token_slice = TokenSlice::new(&tokenizer);

        prog.parse(token_slice).map_err(|e| {
            let offset = e.offset();
            ComRaTTError::from_context_error(e.into_inner(), offset, input)
        })
    }
}

impl<'a> Parser<Input<'a>, &'a Token<'a>, ContextError> for TokenKind {
    fn parse_next(&mut self, input: &mut Input<'a>) -> Result<&'a Token<'a>, ContextError> {
        literal(*self).parse_next(input).map(|t| &t[0])
    }
}

pub fn text<'a, Registry>(
    text: &'static str,
) -> impl Parser<Input<'a>, &'a Token<'a>, ContextError> {
    move |input: &mut Input<'a>| literal(text).parse_next(input).map(|t| &t[0])
}

fn atomic_typ<'a>(input: &mut Input<'a>) -> Result<Type> {
    alt((
        TokenKind::TBool.value(Type::TBool),
        TokenKind::TInt.value(Type::TInt),
        TokenKind::Unit.value(Type::TUnit),
        delimited(TokenKind::LParen, typ, TokenKind::RParen),
    ))
    .context(StrContext::Label("type"))
    .context(StrContext::Expected(StrContextValue::Description(
        "atomic type",
    )))
    .parse_next(input)
}

fn arrow_typ<'a>(input: &mut Input<'a>) -> Result<Type> {
    let t1 = atomic_typ.parse_next(input)?;
    let t2 = opt(preceded(TokenKind::Arrow, arrow_typ)).parse_next(input)?;

    Ok(match t2 {
        Some(t2) => Type::TFun(t1.b(), t2.b()),
        None => t1,
    })
}

fn typ<'a>(input: &mut Input<'a>) -> Result<Type> {
    arrow_typ
        .context(StrContext::Label("type"))
        .context(StrContext::Expected(StrContextValue::Description(
            "type expression",
        )))
        .parse_next(input)
}

fn simple_expr<'a>(input: &mut Input<'a>) -> Result<Expr> {
    alt((
        TokenKind::Ident.map(|t| Expr::Var(t.text().to_owned())),
        TokenKind::Int.try_map(|t| t.text().parse().map(|n| Expr::Const(Const::CInt(n)))),
        TokenKind::True.value(Expr::Const(Const::CBool(true))),
        TokenKind::False.value(Expr::Const(Const::CBool(false))),
        TokenKind::Unit.value(Expr::Const(Const::CUnit)),
        delimited(TokenKind::LParen, expr, TokenKind::RParen),
    ))
    .parse_next(input)
}

fn atomic_expr<'a>(input: &mut Input<'a>) -> Result<Expr> {
    alt((
        preceded(TokenKind::Delay, simple_expr).map(|e| Expr::Delay(e.b())),
        preceded(TokenKind::Advance, TokenKind::Ident).map(|t| Expr::Advance(t.text().to_string())),
        simple_expr,
    ))
    .parse_next(input)
}

fn app_expr<'a>(input: &mut Input<'a>) -> Result<Expr> {
    let first = atomic_expr.parse_next(input)?;

    // Try to parse a sequence of atomic expressions
    let rest = repeat(0.., atomic_expr);

    rest.fold(
        move || first.clone(),
        |acc, expr| Expr::App(acc.b(), expr.b()),
    )
    .parse_next(input)
}

fn mul_op<'a>(input: &mut Input<'a>) -> Result<Binop> {
    alt((
        TokenKind::Times.value(Binop::Mul),
        TokenKind::Div.value(Binop::Div),
    ))
    .parse_next(input)
}

fn add_op<'a>(input: &mut Input<'a>) -> Result<Binop> {
    alt((
        TokenKind::Plus.value(Binop::Add),
        TokenKind::Minus.value(Binop::Sub),
    ))
    .parse_next(input)
}

fn compare<'a>(input: &mut Input<'a>) -> Result<Binop> {
    alt((
        TokenKind::Equal.value(Binop::Eq),
        TokenKind::Lt.value(Binop::Lt),
        TokenKind::Lte.value(Binop::Lte),
        TokenKind::Gt.value(Binop::Gt),
        TokenKind::Gte.value(Binop::Gte),
        TokenKind::Neq.value(Binop::Neq),
    ))
    .parse_next(input)
}

fn mul_expr<'a>(input: &mut Input<'a>) -> Result<Expr> {
    let first = app_expr.parse_next(input)?;

    repeat(0.., (mul_op, app_expr))
        .fold(
            move || first.clone(),
            |acc, (op, expr)| Expr::Prim(op, acc.b(), expr.b()),
        )
        .parse_next(input)
}

fn add_expr<'a>(input: &mut Input<'a>) -> Result<Expr> {
    let first = mul_expr.parse_next(input)?;

    repeat(0.., (add_op, mul_expr))
        .fold(
            move || first.clone(),
            |acc, (op, expr)| Expr::Prim(op, acc.b(), expr.b()),
        )
        .parse_next(input)
}

fn compare_expr<'a>(input: &mut Input<'a>) -> Result<Expr> {
    let first = add_expr.parse_next(input)?;

    repeat(0.., (compare, add_expr))
        .fold(
            move || first.clone(),
            |acc, (op, expr)| Expr::Prim(op, acc.b(), expr.b()),
        )
        .parse_next(input)
}

fn expr<'a>(input: &mut Input<'a>) -> Result<Expr> {
    alt((
        preceded(
            TokenKind::Let,
            (
                TokenKind::Ident,
                preceded(TokenKind::Equal, expr),
                preceded(TokenKind::In, expr),
            ),
        )
        .map(|(x, e1, e2)| Expr::Let(x.text().to_string(), Box::new(e1), Box::new(e2))),
        preceded(
            TokenKind::Fun,
            (
                repeat(0.., TokenKind::Ident)
                    .map(|xs: Vec<&Token>| xs.into_iter().map(|x| x.text().to_string()).collect()),
                preceded(TokenKind::Arrow, expr),
            ),
        )
        .map(|(xs, e)| Expr::Lam(xs, Box::new(e))),
        preceded(
            TokenKind::If,
            (
                compare_expr,
                preceded(TokenKind::Then, compare_expr),
                preceded(TokenKind::Else, compare_expr),
            ),
        )
        .map(|(guard, then_branch, else_branch)| {
            Expr::IfThenElse(
                Box::new(guard),
                Box::new(then_branch),
                Box::new(else_branch),
            )
        }),
        compare_expr,
    ))
    .parse_next(input)
}
// Top-level parsers
fn fundef<'a>(input: &mut Input<'a>) -> Result<Toplevel> {
    let mut parser = terminated(
        (
            TokenKind::Ident.context(StrContext::Expected(StrContextValue::Description(
                "function name",
            ))),
            preceded(TokenKind::Colon, typ).context(StrContext::Expected(
                StrContextValue::Description("function type annotation"),
            )),
            preceded(TokenKind::Let, TokenKind::Ident).context(StrContext::Expected(
                StrContextValue::Description("binding name"),
            )),
            repeat(0.., TokenKind::Ident)
                .map(|xs: Vec<&Token>| xs.into_iter().map(|x| x.text().to_string()).collect())
                .context(StrContext::Expected(StrContextValue::Description(
                    "function parameters",
                ))),
            preceded(TokenKind::Equal, expr).context(StrContext::Expected(
                StrContextValue::Description("function body"),
            )),
        ),
        TokenKind::Semi,
    )
    .map(|(name, t, _def_name, args, e)| Toplevel::FunDef(name.text().to_string(), t, args, e.b()))
    .context(StrContext::Label("function definition"))
    .context(StrContext::Expected(StrContextValue::Description(
        "function definition",
    )));

    parser.parse_next(input)
}

pub fn prog<'a>(input: &mut Input<'a>) -> Result<Prog> {
    terminated(repeat(0.., fundef).map(Prog), TokenKind::EOF)
        .context(StrContext::Label("program"))
        .context(StrContext::Expected(StrContextValue::Description(
            "top level function definition",
        )))
        .parse_next(input)
}

// impl std::str::FromStr for Prog {
//     type Err = ComRaTTError;

//     fn from_str(input: &str) -> Result<Self, Self::Err> {
//         prog.parse(input).map_err(|e| {
//             eprintln!("Full parse error: {:?}", e);
//             ComRaTTError::from_parse(e, input)
//         })
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Tokenizer;

    pub fn tokenize<'a>(input: &'a str) -> Vec<Token<'a>> {
        Tokenizer::tokenize(input)
            .filter_map(|t| t.ok())
            .collect_vec()
    }

    #[test]
    fn test_idents() {
        let tokens = tokenize("x");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            simple_expr.parse_next(&mut token_slice),
            Ok(Expr::Var("x".to_string()))
        );

        let tokens = tokenize("foo");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            simple_expr.parse_next(&mut token_slice),
            Ok(Expr::Var("foo".to_string()))
        );

        let tokens = tokenize("bar123");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            simple_expr.parse_next(&mut token_slice),
            Ok(Expr::Var("bar123".to_string()))
        );

        let tokens = tokenize("foo_bar");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            simple_expr.parse_next(&mut token_slice),
            Ok(Expr::Var("foo_bar".to_string()))
        );

        let tokens = tokenize("_test");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            simple_expr.parse_next(&mut token_slice),
            Ok(Expr::Var("_test".to_string()))
        );

        let tokens = tokenize("");
        let mut token_slice = TokenSlice::new(&tokens);
        assert!(simple_expr.parse_next(&mut token_slice).is_err());
    }

    #[test]
    fn test_atomic_types() {
        let tokens = tokenize("int");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(typ.parse_next(&mut token_slice), Ok(Type::TInt));

        let tokens = tokenize("bool");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(typ.parse_next(&mut token_slice), Ok(Type::TBool));

        let tokens = tokenize("()");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(typ.parse_next(&mut token_slice), Ok(Type::TUnit));

        let tokens = tokenize("(int)");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(typ.parse_next(&mut token_slice), Ok(Type::TInt));
    }

    #[test]
    fn test_arrow_types() {
        let tokens = tokenize("int -> bool");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            typ.parse_next(&mut token_slice),
            Ok(Type::TFun(Box::new(Type::TInt), Box::new(Type::TBool)))
        );

        let tokens = tokenize("int -> bool -> ()");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            typ.parse_next(&mut token_slice),
            Ok(Type::TFun(
                Box::new(Type::TInt),
                Box::new(Type::TFun(Box::new(Type::TBool), Box::new(Type::TUnit)))
            ))
        );
    }

    #[test]
    fn test_app_expr() {
        let tokens = tokenize("f x");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::App(
                Box::new(Expr::Var("f".to_string())),
                Box::new(Expr::Var("x".to_string()))
            ))
        );

        let tokens = tokenize("delay x");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Delay(Box::new(Expr::Var("x".to_string()))))
        );

        let tokens = tokenize("advance x");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Advance("x".to_string()))
        );
    }

    #[test]
    fn test_simple_exprs() {
        let tokens = tokenize("x");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Var("x".to_string()))
        );

        let tokens = tokenize("42");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Const(Const::CInt(42)))
        );

        let tokens = tokenize("true");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Const(Const::CBool(true)))
        );

        let tokens = tokenize("false");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Const(Const::CBool(false)))
        );

        let tokens = tokenize("()");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Const(Const::CUnit))
        );
    }

    #[test]
    fn test_binop_exprs() {
        let tokens = tokenize("1 + 2");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Prim(
                Binop::Add,
                Box::new(Expr::Const(Const::CInt(1))),
                Box::new(Expr::Const(Const::CInt(2)))
            ))
        );
    }

    #[test]
    fn test_let_expr() {
        let tokens = tokenize("let x = 1 in x");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Let(
                "x".to_string(),
                Box::new(Expr::Const(Const::CInt(1))),
                Box::new(Expr::Var("x".to_string()))
            ))
        );
    }

    #[test]
    fn test_lambda_expr() {
        let tokens = tokenize("fun x -> x");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
            Ok(Expr::Lam(
                vec!["x".to_string()],
                Box::new(Expr::Var("x".to_string()))
            ))
        );
    }

    #[test]
    fn test_if_expr() {
        let tokens = tokenize("if true then 1 else 0");
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            expr.parse_next(&mut token_slice),
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
        let tokens = tokenize(input);
        let mut token_slice = TokenSlice::new(&tokens);
        assert_eq!(
            fundef.parse_next(&mut token_slice),
            Ok(Toplevel::FunDef(
                "id".to_string(),
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
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
        let tokens = tokenize(input);
        let mut token_slice = TokenSlice::new(&tokens);
        let result = prog.parse_next(&mut token_slice);
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
            let check_prime_helper n divisor = false;

            main : int -> bool
            let main x = is_prime x;
            "#;

        let tokens = tokenize(input);
        let mut token_slice = TokenSlice::new(&tokens);
        let result = prog.parse_next(&mut token_slice);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().0.len(), 3);
    }
}
