open ComRaTTlib.Source
open ComRaTTlib.Infer
open ComRaTTlib.Preprocess.Lift
open OUnit2

(* Helper function to create test cases *)
let make_test name input expected =
  name
  >:: fun _ ->
  let result, _ = lambda_lift_expr input in
  assert_bool
    ("Expected:\n" ^ show_typed_expr expected ^ "\nBut got:\n" ^ show_typed_expr result)
    (equal_typed_expr result expected)
;;

let tests = "lambda_lifting" >::: []
let () = run_test_tt_main tests
