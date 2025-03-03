#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(assert_matches)]

pub mod anf;
pub mod backend;
pub mod error;
pub mod format;
pub mod infer;
pub mod lexer;
pub mod parse;
pub mod passes;
pub mod range;
pub mod source;
#[cfg(test)]
mod test_suite;
pub mod types;
