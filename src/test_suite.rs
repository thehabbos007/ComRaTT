#![allow(unused)]

use crate::{
    infer::infer_all,
    passes::{
        eliminate_consec_app::EliminateConsecApp, eliminate_partial::PartialElimination,
        lambda_lift::LambdaLift, Pass,
    },
    source::{Prog, Type},
};

struct CompilerPassTest {
    input: &'static str,
    expected: &'static str,
    passes: Vec<Box<dyn Pass>>,
}

impl CompilerPassTest {
    fn new(input: &'static str, expected: &'static str) -> Self {
        Self {
            input,
            expected,
            passes: Vec::new(),
        }
    }

    fn with_pass<P: Pass + 'static>(mut self, pass: P) -> Self {
        self.passes.push(Box::new(pass));
        self
    }

    fn run(&mut self) {
        // Parse and type check the input program
        let input_prog = Prog::parse(self.input).expect("Failed to parse input program");
        let mut typed_prog = infer_all(input_prog);

        // Run passes
        for pass in &mut self.passes {
            typed_prog = pass.run(typed_prog);
        }

        // Parse and type check the expected program
        let expected_prog = Prog::parse(self.expected).expect("Failed to parse expected program");
        let expected_typed = infer_all(expected_prog);

        // Compare string representations
        assert_eq!(
            typed_prog.to_string(),
            expected_typed.to_string(),
            "\nExpected:\n{}\n\nGot:\n{}\n",
            expected_typed.to_string(),
            typed_prog.to_string()
        );
    }
}

// #[test]
fn test_eliminate_partial_application() {
    CompilerPassTest::new(
        // Input program
        r#"
        main : int -> int
        let main x =
            let f = fun y z -> y + z in
            let foo = f 2 in
            foo 3
        ;
        "#,
        // Expected program after transformation
        r#"
        main : int -> int
        let main x =
            let f = fun y z -> y + z in
            let foo = (fun z -> f 2 z) in
            foo 3
        ;
        "#,
    )
    .with_pass(PartialElimination::new())
    .run();
}
