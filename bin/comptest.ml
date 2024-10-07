open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Preprocess

let _let_example =
  ALet
    ( "inc"
    , TInt
    , ALam ([ "x", TInt ], APrim (Add, AVar ("x", TInt), ACstI (1, TInt), TInt), TInt)
    , AApp (AVar ("inc", TInt), [ ACstI (41, TInt) ], TInt) )
;;

(*
   let x = 41 in x+1
   svarer til
   ALet ("x", TInt, ACstI (42, TInt), ALam ([("x", TInt)], APrim(Add, AVar ("x", TInt), ACstI (1, TInt), TInt)))

   men vent, alle lambdaer er i en top level let binding, hvilket vil sige at den har et navn.
   eksempel:

   let x = fun y -> y+1 in x 41
   hvilket vel svarer til

   ALet ("x", TInt, ALam([("y", TInt)], APrim(Add, AVar ("y", TInt), ACstI (1, TInt), TInt))
*)

let _reallet =
  ALet
    ( "x"
    , TInt
    , ALam ([ "y", TInt ], APrim (Add, AVar ("y", TInt), ACstI (1, TInt), TInt), TInt)
    , AApp (AVar ("x", TInt), [ ACstI (42, TInt) ], TInt) )
;;

let _newlet =
  ALet
    ( "x"
    , TInt
    , ACstI (42, TInt)
    , ALam ([ "x", TInt ], APrim (Add, AVar ("x", TInt), ACstI (1, TInt), TInt), TInt) )
;;

let _var_example = AVar ("x", TInt)
let _prim_example = APrim (Add, AVar ("x", TInt), ACstI (41, TInt), TInt)
let _simple_let = ALet ("testfun", TInt, ACstI (42, TInt), ACstI (20, TInt))
(*let () = print_endline (init_wat _reallet []) |> ignore*)
(*
   let _ =  print_endline (init_wat var_example) |> ignore in
   let _ = print_endline (comp prim_example) |> ignore in
   print_endline (init_wat let_example) |> ignore
*)
(*
   let lambda = ALam ([("x", TInt); ("y", TInt)], APrim(Add, AVar ("x", TInt), AVar ("y", TInt), TInt)) in
   print_endline (init_wat lambda) |> ignore
*)
(*
   let add = APrim (Add, ACstI (42, TInt), ACstI (42, TInt), TInt) in
   print_endline (init_wat add) |> ignore
*)
let _correct_args = 
   AApp (
     ALam ([("x", TInt); ("y", TInt)], 
           APrim (Add, AVar ("x", TInt), AVar ("y", TInt), TInt),
           TArrow (TInt, TArrow (TInt, TInt))),
     [ACstI (5, TInt); ACstI (3, TInt)],
     TInt
   )
(*
let () = 
_correct_args 
|> part_elim 
|> show_annot_expr 
|> print_endline
*)

let _add42 = ALam ([("x", TInt)], APrim(Add, AVar ("x", TInt), ACstI (42, TInt), TInt), TArrow (TInt, TInt))
let _applied = AApp (_add42, [ACstI (42, TInt)], TInt)

let _add42raw = Lam ("x", Prim(Add, Var "x", CstI 42))
let _appraw = App (_add42raw, CstI 42)

let _nested = Lam ("x", Lam ("y", Prim(Add, Var "x", Var "y")))

(*
let _ =
   let (_lifted, _globals) = Lift.lambda_lift_expr [] _add42 in
   (*print_endline (show_annot_expr _lifted)*)
   let head = List.hd _globals in
   let { body; _ } = head in
   print_endline (show_annot_expr body)*)

let _show_tup tup = 
   let (int, typ) = tup in
   print_endline (Printf.sprintf "%s %s" (string_of_int int) (show_typ typ))

let _show_global global = 
   let { body; _} = global in
   print_endline ("> " ^ (show_annot_expr body))

let () = 
   (*let (_subst, _annot, _ty) = annotate [] [] _nested in
   (*List.map show_tup _subst |> ignore*)
   print_endline (show_annot_expr _annot)
   *)
   let (_subst, _annot, _ty) = annotate [] [] _nested in
   let (_annot, _globals) = Lift.lambda_lift_expr [] _annot in
   print_endline (show_annot_expr _annot);
   print_endline "separator";
   List.map _show_global _globals |> ignore


