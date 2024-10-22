open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Compile
open ComRaTTlib.Interpret

let _run_example ast ~(env : value Environment.t) =
  let _, annotated, _ = annotate [ "x", TInt ] [] ast in
  let value = interp annotated env in
  print_endline ("Result: " ^ string_of_value value);
  annotated
;;

let _name = "prog.wat"

let _example =
  ALet
    ( "x"
    , TInt
    , ALam
        ([ "y", TInt ], APrim (Add, AVar ("y", TInt), AConst (CInt 1, TInt), TInt), TInt)
    , AApp (AVar ("x", TInt), [ AConst (CInt 42, TInt) ], TInt) )
;;

(* woops, we cannot apply with multiple arguments..
   YES! nested applications, but we do not have nested lambdas?

   let add = fun x -> fun y -> x+y in add 41 1
   -> we do not support this.
*)
let _example2 =
  ALet
    ( "add"
    , TInt
    , ALam
        ( [ "y", TInt; "z", TInt ]
        , APrim (Add, AVar ("y", TInt), AVar ("z", TInt), TInt)
        , TInt )
    , AApp (AVar ("x", TInt), [ AConst (CInt 42, TInt) ], TInt) )
;;

(*
   let example2better = ALet ("add", TInt, ALam([("x", TInt); ("y", TInt)], APrim(Add, AVar ("x", TInt), AVar ("y", TInt), TInt)), AApp (AVar ("add", TInt), AApp (AVar ("y", TInt), ACstI (42), TInt), TInt));;
*)

let _multiarg =
  ALet
    ( "add"
    , TInt
    , ALam
        ( [ "x", TInt; "y", TInt ]
        , APrim (Add, AVar ("x", TInt), AVar ("y", TInt), TInt)
        , TInt )
    , AApp (AVar ("add", TInt), [ AVar ("x", TInt); AVar ("y", TInt) ], TInt) )
;;

let _multiargCsti =
  ALet
    ( "add"
    , TInt
    , ALam
        ( [ "x", TInt; "y", TInt ]
        , APrim (Add, AVar ("x", TInt), AVar ("y", TInt), TInt)
        , TInt )
    , AApp (AVar ("add", TInt), [ AConst (CInt 41, TInt); AConst (CInt 1, TInt) ], TInt)
    )
;;

let _justApp = AApp (AVar ("add", TInt), [], TInt)

let _let_example =
  Let
    ( "inc"
    , Lam ([ "x" ], Prim (Add, Var "x", Const (CInt 1)))
    , Let ("x", Const (CInt 5), App (Var "inc", Var "x")) )
;;

(*let _let_example_annotated = run_example _let_example ~env:Environment.empty;;*)
let _program = init_wat _multiargCsti []

let () =
  (*print_endline (_program)*)
  (*let _ = print_endline (show_annot_expr _let_example_annotated) |> ignore in ()*)
  let oc = open_out _name in
  Printf.fprintf oc "%s" _program;
  close_out oc;
  let args = Sys.argv |> Array.to_list |> List.tl |> String.concat " " in
  Sys.command (Printf.sprintf "wasmer %s --invoke caller %s" _name args) |> ignore
;;
