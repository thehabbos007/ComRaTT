open ComRaTTlib.Source
open ComRaTTlib.Annotate
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

let tests =
  "lambda_lifting"
  >::: [ (* Test 1: Simple constant - should remain unchanged *)
         make_test "constant" (TConst (CInt 42, TInt)) (TConst (CInt 42, TInt))
       ; (* Test 2: Simple variable - should remain unchanged *)
         make_test "variable" (TName ("x", TInt)) (TName ("x", TInt))
       ; (* Test 3: Lambda with no free variables - should remain unchanged *)
         make_test
           "lambda_no_free_vars"
           (TLet
              ( "y"
              , TInt
              , TLam ([ "x", TInt ], TName ("x", TInt), TInt)
              , TApp (TName ("x", TInt), [ TConst (CInt 5, TInt) ], TInt) ))
           (TLet
              ( "y"
              , TInt
              , TLam ([ "x", TInt ], TName ("x", TInt), TInt)
              , TApp (TName ("x", TInt), [ TConst (CInt 5, TInt) ], TInt) ))
       ; (* Test 4: Lambda with a free variable *)
         make_test
           "lambda_with_free_var"
           (TLet
              ( "y"
              , TInt
              , TConst (CInt 5, TInt)
              , TLam
                  ( [ "x", TInt ]
                  , TPrim (Add, TName ("x", TInt), TName ("y", TInt), TInt)
                  , TInt ) ))
           (TLet
              ( "y"
              , TInt
              , TConst (CInt 5, TInt)
              , TApp
                  ( TName ("#global_lam_1", TInt)
                  , [ TName ("y", TInt) ]
                  , TArrow (TInt, TInt) ) ))
       ; (* Test 5: Nested lambdas *)
         make_test
           "nested_lambdas"
           (TLet
              ( "nested"
              , TInt
              , TLam
                  ( [ "x", TInt ]
                  , TLam
                      ( [ "y", TInt ]
                      , TPrim (Add, TName ("x", TInt), TName ("y", TInt), TInt)
                      , TInt )
                  , TInt )
              , TApp (TName ("nested", TInt), [ TConst (CInt 1, TInt) ], TInt) ))
           (TLet
              ( "nested"
              , TInt
              , TLam
                  ( [ "x", TInt ]
                  , TApp
                      ( TName ("#global_lam_1", TInt)
                      , [ TName ("x", TInt) ]
                      , TArrow (TInt, TInt) )
                  , TInt )
              , TApp (TName ("nested", TInt), [ TConst (CInt 1, TInt) ], TInt) ))
       ; (* Test 6: Multiple free variables *)
         make_test
           "multiple_free_vars"
           (TLet
              ( "a"
              , TInt
              , TConst (CInt 1, TInt)
              , TLet
                  ( "b"
                  , TInt
                  , TConst (CInt 2, TInt)
                  , TLam
                      ( [ "x", TInt ]
                      , TPrim
                          ( Add
                          , TPrim (Add, TName ("x", TInt), TName ("a", TInt), TInt)
                          , TName ("b", TInt)
                          , TInt )
                      , TInt ) ) ))
           (TLet
              ( "a"
              , TInt
              , TConst (CInt 1, TInt)
              , TLet
                  ( "b"
                  , TInt
                  , TConst (CInt 2, TInt)
                  , TApp
                      ( TName ("#global_lam_1", TInt)
                      , [ TName ("a", TInt); TName ("b", TInt) ]
                      , TArrow (TInt, TInt) ) ) ))
       ; (* Test 7: Function application *)
         make_test
           "function_application"
           (TApp
              ( TLam ([ "x", TInt ], TName ("x", TInt), TInt)
              , [ TConst (CInt 42, TInt) ]
              , TInt ))
           (TApp
              ( TApp (TName ("#global_lam_1", TInt), [], TArrow (TInt, TInt))
              , [ TConst (CInt 42, TInt) ]
              , TInt ))
         (* This test is a little funky, we should technically not allow this? *)
         (* ; (* Test 8: Let binding with lambda in body *)
            make_test
            "let_with_lambda"
            (TLet
            ( "z"
            , TInt
            , ACstI (10, TInt)
            , TLam
            ( [ "x", TInt ]
            , TPrim (Add, TName ("x", TInt), TName ("z", TInt), TInt)
            , TInt ) ))
            (TLet
            ( "z"
            , TInt
            , ACstI (10, TInt)
            , TApp
            (* This below is wrong because we return a curried function,
            which we don't support in wasm *)
            (TName ("#global_lam_1", TInt), [ TName ("z", TInt) ], TArrow (TInt, TInt))
            )) *)
       ]
;;

let () = run_test_tt_main tests
