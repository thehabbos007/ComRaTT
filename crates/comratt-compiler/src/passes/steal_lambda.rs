use crate::{
    passes::Pass,
    types::{TypedExpr, TypedProg, TypedToplevel},
};


/// Steal lambda from top level def:
///
///   def f x = fun y z -> e
/// transforms to
///   def f x y z = e
#[derive(Debug, Default)]
pub struct StealLambda;

impl StealLambda {
    pub fn new() -> Self {
        Self
    }
}

impl Pass for StealLambda {
    fn run(&mut self, prog: TypedProg) -> TypedProg {
        let defs = prog
            .defs
            .into_iter()
            .map(|def| match def {
                TypedToplevel::TFunDef(name, mut args, body, _) => {
                    let mut body = *body;
                    while let TypedExpr::TLam(lam_args, lam_body, _, _) = body {
                        args.extend(lam_args);
                        body = *lam_body;
                    }
                    let ret_ty = body.ty();
                    TypedToplevel::TFunDef(name, args, Box::new(body), ret_ty)
                }
                other => other,
            })
            .collect();

        TypedProg {
            defs,
            sorted_inputs: prog.sorted_inputs,
        }
    }
}
