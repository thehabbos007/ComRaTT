use std::{assert_matches::assert_matches, collections::HashSet, sync::LazyLock};

use itertools::Itertools;
use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{self, Assoc, Op, PrattParser},
    Parser,
};
use pest_derive::Parser;

use crate::{
    error::ComRaTTError,
    range::Range,
    source::{Binop, Const, Expr, Prog, Toplevel, Type},
};

#[derive(Parser)]
#[grammar = "lang.pest"]
pub struct ComRaTTParser;

static BINOP_PARSER: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::equality_op, Assoc::Left))
        .op(Op::infix(Rule::relational_op, Assoc::Left))
        .op(Op::infix(Rule::add_op, Assoc::Left))
        .op(Op::infix(Rule::mul_op, Assoc::Left))
});

static TYPE_PARSER: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        // .op(Op::prefix(rule))
        .op(Op::infix(Rule::arrow, Assoc::Right))
});

impl Prog {
    pub fn parse(input: &str) -> Result<Prog, String> {
        let pairs = match ComRaTTParser::parse(Rule::program, input) {
            Ok(pairs) => pairs,
            Err(error) => {
                let error = miette::Error::new(error.into_miette());
                return Err(format!("{error:?}"));
            }
        };

        let mut toplevels = Vec::new();
        for pair in pairs {
            match pair.as_rule() {
                Rule::function_def => {
                    let mut pairs = pair.into_inner();
                    let name = pairs.next().unwrap().as_str().to_string();
                    let typ = parse_type(pairs.next().unwrap().into_inner());
                    let decl_name = pairs.next().unwrap();

                    if decl_name.as_str() != name {
                        panic!(
                            "Expected name of toplevel to match its definition, {name} {decl_name}"
                        );
                    }

                    // Parse arguments (they may be absent)
                    let args = pairs
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|arg| arg.as_str().to_string())
                        .collect_vec();

                    // The last item should be the expression
                    let expr = parse_expression(pairs.next().unwrap().into_inner());

                    toplevels.push(Toplevel::FunDef(name, typ, args, expr));
                }
                Rule::channel_def => {
                    let mut pairs = pair.into_inner();
                    let name = pairs.next().unwrap().as_str().to_string();
                    let typ = parse_type(pairs.next().unwrap().into_inner());
                    toplevels.push(Toplevel::Channel(name, typ));
                }
                Rule::output_def => {
                    let mut pairs = pair.into_inner();
                    let name = pairs.next().unwrap().as_str().to_string();
                    let expr = parse_expression(pairs.next().unwrap().into_inner());
                    toplevels.push(Toplevel::Output(name, expr));
                }
                Rule::EOI => break,
                x => panic!("Entered unreachable: {x:?}"),
            }
        }

        Ok(Prog(toplevels))
    }
}

fn parse_type(pairs: Pairs<Rule>) -> Type {
    TYPE_PARSER
        .map_primary(|primary| parse_type_atom(primary))
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::arrow => Type::TFun(Box::new(lhs), Box::new(rhs)),
            _ => unreachable!(),
        })
        .parse(pairs)
}

fn parse_type_atom(pair: Pair<Rule>) -> Type {
    match pair.as_rule() {
        Rule::int_type => Type::TInt,
        Rule::bool_type => Type::TBool,
        Rule::unit_type => Type::TUnit,
        Rule::type_expr => parse_type(pair.into_inner()),
        Rule::parenthesis_or_tuple_type => {
            let mut typs = pair
                .into_inner()
                .map(|typ| parse_type(typ.into_inner()))
                .collect_vec();

            if typs.len() == 1 {
                return typs.remove(0);
            }

            Type::TProduct(typs)
        }
        _ => unreachable!("Unknown atomic type: {:?}", pair.as_rule()),
    }
}

fn parse_expression(pairs: Pairs<Rule>) -> Expr {
    BINOP_PARSER
        .map_primary(|primary| parse_expression_atom(primary))
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_str() {
                "<" => Binop::Lt,
                "<=" => Binop::Lte,
                ">" => Binop::Gt,
                ">=" => Binop::Gte,
                "=" => Binop::Eq,
                "<>" => Binop::Neq,
                "+" => Binop::Add,
                "-" => Binop::Sub,
                "*" => Binop::Mul,
                "/" => Binop::Div,
                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };

            Expr::Prim(op, Box::new(lhs), Box::new(rhs))
        })
        .parse(pairs)
}

fn parse_expression_atom(pair: Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::let_expr => {
            let mut pairs = pair.into_inner();
            let var_name = pairs.next().unwrap().as_str().to_string();
            let bind_expr = parse_expression(pairs.next().unwrap().into_inner());
            let body_expr = parse_expression(pairs.next().unwrap().into_inner());
            Expr::Let(var_name, Box::new(bind_expr), Box::new(body_expr))
        }
        Rule::fun_expr => {
            let mut pairs = pair.into_inner();

            // Parse arguments (they may be absent)
            let args = pairs
                .next()
                .unwrap()
                .into_inner()
                .map(|arg| arg.as_str().to_string())
                .collect_vec();

            // The body expression
            let body = parse_expression(pairs.next().unwrap().into_inner());
            Expr::Lam(args, Box::new(body))
        }
        Rule::if_expr => {
            let mut pairs = pair.into_inner();
            let cond = parse_expression(pairs.next().unwrap().into_inner());
            let then_branch = parse_expression(pairs.next().unwrap().into_inner());
            let else_branch = parse_expression(pairs.next().unwrap().into_inner());
            Expr::IfThenElse(Box::new(cond), Box::new(then_branch), Box::new(else_branch))
        }

        Rule::delay_expr => {
            let mut delay_inner = pair.into_inner();
            let mut channel_set = HashSet::new();

            for channel in delay_inner.next().unwrap().into_inner() {
                channel_set.insert(channel.as_str().to_string());
            }

            let expr = parse_expression(delay_inner.next().unwrap().into_inner());
            Expr::Delay(Box::new(expr), channel_set)
        }
        Rule::advance_expr => {
            let channel = pair.into_inner().next().unwrap().as_str().to_string();
            Expr::Advance(channel)
        }
        Rule::wait_expr => {
            let channel = pair.into_inner().next().unwrap().as_str().to_string();
            Expr::Wait(channel)
        }
        Rule::parenthesis_or_tuple => {
            let mut exprs = pair
                .into_inner()
                .map(|expr| parse_expression(expr.into_inner()))
                .collect_vec();

            if exprs.len() == 1 {
                return exprs.remove(0);
            }

            Expr::Tuple(exprs)
        }
        Rule::integer => Expr::Const(Const::CInt(pair.as_str().parse().unwrap())),
        Rule::true_lit => Expr::Const(Const::CBool(true)),
        Rule::false_lit => Expr::Const(Const::CBool(false)),
        Rule::unit_lit => Expr::Const(Const::CUnit),
        Rule::identifier => Expr::Var(pair.as_str().to_string()),
        Rule::term => pair
            .into_inner()
            .map(|e| parse_term(e))
            .reduce(|e1, e2| Expr::App(e1.b(), e2.b()))
            .expect("At least one term"),
        Rule::expr => parse_expression(pair.into_inner()),
        _ => unreachable!("Unknown expression type: {:?}", pair.as_rule()),
    }
}

fn parse_term(pair: Pair<Rule>) -> Expr {
    assert_matches!(pair.as_rule(), Rule::tuple_access);

    let mut pairs = pair.into_inner();
    let mut expr = parse_expression_atom(pairs.next().unwrap());

    while let Some(tuple_access_index) = pairs.next()
        && tuple_access_index.as_rule() == Rule::integer
    {
        let index = tuple_access_index
            .as_str()
            .parse::<i32>()
            .expect("tuple access index to be an integer");
        expr = Expr::Access(Box::new(expr), index);
    }
    expr
}
