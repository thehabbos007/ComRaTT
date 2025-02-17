use crate::infer::TypedProg;

pub mod eliminate_partial;

pub trait Pass<I = TypedProg, O = TypedProg> {
    fn run(&mut self, prog: I) -> O;
}
