open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Interpret

let run_example ast ~(env : value Environment.t) =
  let _, annotated, _ = annotate [ "x", TInt ] [] ast in
  let value = interp annotated env in
  print_endline ("Result: " ^ string_of_value value);
  annotated
;;

let id = Lam ("x", Var "x")
let square_fun = Lam ("x", Prim (Mul, Var "x", Var "x"))

let () =
  let _example = App (id, CstI 84) |> run_example ~env:Environment.empty in
  let _add_example = Prim (Add, CstI 42, CstI 42) |> run_example ~env:Environment.empty in
  let _sub_example = Prim (Sub, CstI 42, CstI 42) |> run_example ~env:Environment.empty in
  let let_example =
    Let
      ( "inc"
      , Lam ("x", Prim (Add, Var "x", CstI 1))
      , Let ("x", CstI 5, App (Var "inc", Var "x")) )
  in
  let _let_example_annotated = run_example let_example ~env:Environment.empty in
  let _binop_with_defined_variable_does_not_fail =
    run_example
      (Prim (Add, CstI 42, Var "x"))
      ~env:(Environment.add "x" (VInt 42) Environment.empty)
  in
  let _binop_with_lambda_fails = run_example (Prim (Add, CstI 42, Lam ("x", CstI 2))) in
  let _five_squared = App (square_fun, CstI 5) |> run_example ~env:Environment.empty in
  run_example
    (Prim (Add, CstI 42, Var "x"))
    ~env:(Environment.add "y" (VInt 42) Environment.empty)
  |> ignore
;;
