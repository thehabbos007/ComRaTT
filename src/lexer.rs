use crate::{error::ComRaTTError, range::Range};

use logos::{Lexer, Logos};
use std::fmt;
use winnow::stream::Location;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub source: &'a str,
    pub kind: TokenKind,
    pub span: Range,
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} [{:?} {:?}]", self.text(), self.kind, self.span)
    }
}

impl<'a> Token<'a> {
    pub fn new_eof(source: &'a str) -> Self {
        Token {
            source,
            kind: TokenKind::EOF,
            span: (source.len()..source.len()).into(),
        }
    }
    pub fn text(&self) -> &'a str {
        &self.source[std::ops::Range::from(self.span)]
    }
}

impl PartialEq<&str> for Token<'_> {
    fn eq(&self, other: &&str) -> bool {
        self.text() == *other
    }
}

impl PartialEq<TokenKind> for Token<'_> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}

impl<'a> Location for Token<'a> {
    #[inline(always)]
    fn current_token_start(&self) -> usize {
        self.span.start()
    }

    #[inline(always)]
    fn previous_token_end(&self) -> usize {
        self.span.end()
    }
}

pub struct Tokenizer<'a> {
    source: &'a str,
    lexer: Lexer<'a, TokenKind>,
    eof: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Tokenizer {
            source,
            lexer: TokenKind::lexer(source),
            eof: false,
        }
    }
}
impl<'a> Tokenizer<'a> {
    pub fn tokenize(source: &'a str) -> impl Iterator<Item = Result<Token<'a>, ComRaTTError<'a>>> {
        Tokenizer::new(source)
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, ComRaTTError<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(Err(_)) => {
                let span = Range::from(self.lexer.span().start..self.source.len());
                let message = "failed to recognize the rest tokens".to_owned();
                Some(Err(ComRaTTError::from_span(message, span, &self.source)))
            }
            Some(Ok(kind)) => Some(Ok(Token {
                source: self.source,
                kind,
                span: self.lexer.span().into(),
            })),
            None => {
                if !self.eof {
                    self.eof = true;
                    Some(Ok(Token::new_eof(self.source)))
                } else {
                    None
                }
            }
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(logos::Logos, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    #[regex("-?(0|[1-9][0-9]*)")]
    Int,

    #[token("fun")]
    Fun,

    #[token("let")]
    Let,

    #[token("def")]
    Def,

    #[token("chan")]
    Chan,

    #[token("in")]
    In,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("delay")]
    Delay,

    #[token("advance")]
    Advance,

    #[token("if")]
    If,

    #[token("then")]
    Then,

    #[token("else")]
    Else,

    #[token("int")]
    TInt,

    #[token("bool")]
    TBool,

    #[token("()")]
    Unit,

    #[token("+")]
    Plus,

    #[token("/")]
    Div,

    #[token("*")]
    Times,

    #[token("-")]
    Minus,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("=")]
    Equal,

    #[token("->")]
    RArrow,

    #[token("<-")]
    LArrow,

    #[token(":")]
    Colon,

    #[token(";")]
    Semi,

    #[token("<")]
    Lt,

    #[token("<=")]
    Lte,

    #[token(">")]
    Gt,

    #[token(">=")]
    Gte,

    #[token("<>")]
    Neq,

    EOF,
}
