open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Preprocess.Lift
open OUnit2

(* Helper function to create test cases *)
let make_test name input expected =
  name
  >:: fun _ ->
  let result, _ = lambda_lift_expr [] input in
  assert_bool
    ("Expected:\n" ^ show_annot_expr expected ^ "\nBut got:\n" ^ show_annot_expr result)
    (equal_annot_expr result expected)
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
              , AApp
                  (AVar ("#global_lam_1", TInt), [ AVar ("y", TInt) ], TArrow (TInt, TInt))
              ))
       ; (* Test 5: Nested lambdas *)
         make_test
           "nested_lambdas"
           (ALet
              ( "nested"
              , TInt
              , ALam
                  ( [ "x", TInt ]
                  , ALam
                      ( [ "y", TInt ]
                      , APrim (Add, AVar ("x", TInt), AVar ("y", TInt), TInt)
                      , TInt )
                  , TInt )
              , AApp (AVar ("nested", TInt), [ ACstI (1, TInt) ], TInt) ))
           (ALet
              ( "nested"
              , TInt
              , ALam
                  ( [ "x", TInt ]
                  , AApp
                      ( AVar ("#global_lam_1", TInt)
                      , [ AVar ("x", TInt) ]
                      , TArrow (TInt, TInt) )
                  , TInt )
              , AApp (AVar ("nested", TInt), [ ACstI (1, TInt) ], TInt) ))
       ; (* Test 6: Multiple free variables *)
         make_test
           "multiple_free_vars"
           (ALet
              ( "a"
              , TInt
              , ACstI (1, TInt)
              , ALet
                  ( "b"
                  , TInt
                  , ACstI (2, TInt)
                  , ALam
                      ( [ "x", TInt ]
                      , APrim
                          ( Add
                          , APrim (Add, AVar ("x", TInt), AVar ("a", TInt), TInt)
                          , AVar ("b", TInt)
                          , TInt )
                      , TInt ) ) ))
           (ALet
              ( "a"
              , TInt
              , ACstI (1, TInt)
              , ALet
                  ( "b"
                  , TInt
                  , ACstI (2, TInt)
                  , AApp
                      ( AVar ("#global_lam_1", TInt)
                      , [ AVar ("a", TInt); AVar ("b", TInt) ]
                      , TArrow (TInt, TInt) ) ) ))
       ; (* Test 7: Function application *)
         make_test
           "function_application"
           (AApp (ALam ([ "x", TInt ], AVar ("x", TInt), TInt), [ ACstI (42, TInt) ], TInt))
           (AApp
              ( AApp (AVar ("#global_lam_1", TInt), [], TArrow (TInt, TInt))
              , [ ACstI (42, TInt) ]
              , TInt ))
         (* This test is a little funky, we should technically not allow this? *)
         (* ; (* Test 8: Let binding with lambda in body *)
            make_test
            "let_with_lambda"
            (ALet
            ( "z"
            , TInt
            , ACstI (10, TInt)
            , ALam
            ( [ "x", TInt ]
            , APrim (Add, AVar ("x", TInt), AVar ("z", TInt), TInt)
            , TInt ) ))
            (ALet
            ( "z"
            , TInt
            , ACstI (10, TInt)
            , AApp
            (* This below is wrong because we return a curried function,
            which we don't support in wasm *)
            (AVar ("#global_lam_1", TInt), [ AVar ("z", TInt) ], TArrow (TInt, TInt))
            )) *)
       ]
;;

let () = run_test_tt_main tests
