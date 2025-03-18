use crate::{anf::AnfProg, types::TypedProg};

pub mod anf;
pub mod eliminate_consec_app;
pub mod eliminate_partial;
pub mod lambda_lift;

pub trait Pass<I = TypedProg, O = TypedProg> {
    fn run(&mut self, prog: I) -> O;
}

pub fn run_program_passes(prog: TypedProg) -> TypedProg {
    eprintln!("Base:\n{prog}");
    let mut eliminate_partial = eliminate_partial::PartialElimination::new();
    let prog = eliminate_partial.run(prog);
    eprintln!("PartialElim:\n{prog}");

    // Let's not do this as it's problematic for us to get rid of partial applications
    // when calling closures
    let mut eliminate_consec = eliminate_consec_app::EliminateConsecApp::new();
    let prog = eliminate_consec.run(prog);
    eprintln!("ConsecElim:\n{prog}");

    let mut lambda_lift = lambda_lift::LambdaLift::new();
    let prog = lambda_lift.run(prog);

    let mut eliminate_partial = eliminate_partial::PartialElimination::new();
    let prog = eliminate_partial.run(prog);

    eprintln!("LamLiftPartialElim:\n{prog}");

    prog
}

pub fn run_program_passes_anf(prog: TypedProg) -> AnfProg {
    let prog = run_program_passes(prog);

    let mut anf = anf::ANFConversion::new();
    let prog = anf.run(prog);

    eprintln!("Anf:\n{prog}");

    prog
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{Binop, Const, Type};
    use crate::types::{TypedExpr, TypedToplevel};

    #[test]
    fn test_run_program_passes_partial_application() {
        // let f = g 1 in f 2 -> let f = (fun x -> g 1 x) in f 2
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_owned(),
            vec![],
            TypedExpr::TLet(
                "f".to_owned(),
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
                TypedExpr::TApp(
                    TypedExpr::TName(
                        "g".to_owned(),
                        Type::TFun(
                            Type::TInt.b(),
                            Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
                        ),
                    )
                    .b(),
                    vec![TypedExpr::TConst(Const::CInt(1), Type::TInt)],
                    Type::TFun(Type::TInt.b(), Type::TInt.b()),
                )
                .b(),
                TypedExpr::TApp(
                    TypedExpr::TName("f".to_owned(), Type::TFun(Type::TInt.b(), Type::TInt.b()))
                        .b(),
                    vec![TypedExpr::TConst(Const::CInt(2), Type::TInt)],
                    Type::TInt,
                )
                .b(),
            )
            .b(),
            Type::TInt,
        )]);

        let result = run_program_passes(prog);

        match &result.0[0] {
            TypedToplevel::TFunDef(_, _, body, _) => match **body {
                TypedExpr::TLet(_, _, box TypedExpr::TLam(_, _, _), _) => (),
                _ => panic!("Expected lambda in let binding after partial elimination"),
            },
            _ => panic!("Expected function definition"),
        }
    }

    #[test]
    fn test_run_program_passes_lambda_lifting() {
        // fun x -> (fun y -> x + y) -> lifted function + closure
        let prog = TypedProg(vec![TypedToplevel::TFunDef(
            "main".to_owned(),
            vec![("x".to_owned(), Type::TInt)],
            TypedExpr::TLam(
                vec![("y".to_owned(), Type::TInt)],
                TypedExpr::TPrim(
                    Binop::Add,
                    TypedExpr::TName("x".to_owned(), Type::TInt).b(),
                    TypedExpr::TName("y".to_owned(), Type::TInt).b(),
                    Type::TInt,
                )
                .b(),
                Type::TFun(Type::TInt.b(), Type::TInt.b()),
            )
            .b(),
            Type::TFun(
                Type::TInt.b(),
                Type::TFun(Type::TInt.b(), Type::TInt.b()).b(),
            ),
        )]);

        let result = run_program_passes(prog);

        assert!(
            result.0.len() > 1,
            "Expected lifted function in addition to main"
        );
    }
}
