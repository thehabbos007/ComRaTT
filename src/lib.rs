#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(assert_matches)]

pub mod anf;
pub mod backend;
pub mod constants;
pub mod format;
pub mod infer;
pub mod parse;
pub mod passes;
pub mod range;
pub mod runtime;
pub mod source;
#[cfg(test)]
mod test_suite;
pub mod types;
