use crate::types::TypedProg;

pub mod eliminate_consec_app;
pub mod eliminate_partial;
pub mod lambda_lift;

pub trait Pass<I = TypedProg, O = TypedProg> {
    fn run(&mut self, prog: I) -> O;
}
