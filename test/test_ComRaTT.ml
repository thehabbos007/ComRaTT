open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Compile
open OUnit2

(* Helper function to compare expressions ignoring types *)
let rec expr_equal e1 e2 =
  match e1, e2 with
  | ACstI (n1, _), ACstI (n2, _) -> n1 = n2
  | AVar (x1, _), AVar (x2, _) -> x1 = x2
  | ALam (params1, body1, t1), ALam (params2, body2, t2) ->
    List.length params1 = List.length params2
    && List.for_all2 (fun (x1, _) (x2, _) -> x1 = x2) params1 params2
    && expr_equal body1 body2
    && t1 == t2
  | AApp (e11, e12, _), AApp (e21, e22, _) ->
    expr_equal e11 e21 && List.for_all2 expr_equal e12 e22
  | APrim (op1, e11, e12, _), APrim (op2, e21, e22, _) ->
    op1 = op2 && expr_equal e11 e21 && expr_equal e12 e22
  | ALet (x1, _, e11, e12), ALet (x2, _, e21, e22) ->
    x1 = x2 && expr_equal e11 e21 && expr_equal e12 e22
  | _ -> false
;;

(* Helper function to create test cases *)
let make_test name input expected =
  name
  >:: fun _ ->
  let result, _ = lambda_lift_expr [] input in
  assert_bool
    ("Expected:\n" ^ show_annot_expr expected ^ "\nBut got:\n" ^ show_annot_expr result)
    (expr_equal result expected)
;;

let tests =
  "lambda_lifting"
  >::: [ (* Test 1: Simple constant - should remain unchanged *)
         make_test "constant" (ACstI (42, TInt)) (ACstI (42, TInt))
       ; (* Test 2: Simple variable - should remain unchanged *)
         make_test "variable" (AVar ("x", TInt)) (AVar ("x", TInt))
       ; (* Test 3: Lambda with no free variables - should remain unchanged *)
         make_test
           "lambda_no_free_vars"
           (ALet
              ( "y"
              , TInt
              , ALam ([ "x", TInt ], AVar ("x", TInt), TInt)
              , AApp (AVar ("x", TInt), [ ACstI (5, TInt) ], TInt) ))
           (ALet
              ( "y"
              , TInt
              , ALam ([ "x", TInt ], AVar ("x", TInt), TInt)
              , AApp (AVar ("x", TInt), [ ACstI (5, TInt) ], TInt) ))
       ; (* Test 4: Lambda with a free variable *)
         make_test
           "lambda_with_free_var"
           (ALet
              ( "y"
              , TInt
              , ACstI (5, TInt)
              , ALam
                  ( [ "x", TInt ]
                  , APrim (Add, AVar ("x", TInt), AVar ("y", TInt), TInt)
                  , TInt ) ))
           (ALet
              ( "y"
              , TInt
              , ACstI (5, TInt)
              , AApp (AVar ("#global_lam_1", TInt), [ AVar ("y", TInt) ], TInt) ))
       ]
;;

let () = run_test_tt_main tests
