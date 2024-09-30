open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Compile

let _e1 () =
  let let_binding =
    Let
      ( "inc"
      , Lam ("x", Prim (Add, Var "x", CstI 1))
      , Let
          ( "dec"
          , Lam ("x", Prim (Sub, Var "x", CstI 1))
          , Let ("x", CstI 5, App (Var "inc", Var "x")) ) )
  in
  let _, annotated, _ = annotate [] [] let_binding in
  let renamed = lambda_lift_rename annotated in
  show_annot_expr annotated |> print_endline;
  print_endline "Annotated V";
  show_annot_expr renamed |> print_endline
;;

let e2 () =
  let let_binding =
    Let
      ( "inc"
      , Lam ("x", Prim (Add, Var "x", CstI 1))
      , Let
          ( "inc"
          , Lam ("x", Prim (Add, Var "x", CstI 1))
          , Let ("x", CstI 5, App (Var "inc", Var "x")) ) )
  in
  let _, annotated, _ = annotate [] [] let_binding in
  let renamed = lambda_lift_rename annotated in
  show_annot_expr annotated |> print_endline;
  print_endline "Annotated V";
  show_annot_expr renamed |> print_endline
;;

let () =
  (* e1 (); *)
  print_endline "=====================";
  e2 ()
;;
