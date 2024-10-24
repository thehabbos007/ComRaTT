open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Preprocess
open ComRaTTlib.Ast_of_text
open ComRaTTlib.Compile

let _let_example =
  ALet
    ( "inc"
    , TInt
    , ALam
        ([ "x", TInt ], APrim (Add, AVar ("x", TInt), AConst (CInt 1, TInt), TInt), TInt)
    , AApp (AVar ("inc", TInt), [ AConst (CInt 41, TInt) ], TInt) )
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
    , ALam
        ([ "y", TInt ], APrim (Add, AVar ("y", TInt), AConst (CInt 1, TInt), TInt), TInt)
    , AApp (AVar ("x", TInt), [ AConst (CInt 42, TInt) ], TInt) )
;;

let _newlet =
  ALet
    ( "x"
    , TInt
    , AConst (CInt 42, TInt)
    , ALam
        ([ "x", TInt ], APrim (Add, AVar ("x", TInt), AConst (CInt 1, TInt), TInt), TInt)
    )
;;

let _var_example = AVar ("x", TInt)
let _prim_example = APrim (Add, AVar ("x", TInt), AConst (CInt 41, TInt), TInt)
let _simple_let = ALet ("testfun", TInt, AConst (CInt 42, TInt), AConst (CInt 20, TInt))

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
  AApp
    ( ALam
        ( [ "x", TInt; "y", TInt ]
        , APrim (Add, AVar ("x", TInt), AVar ("y", TInt), TInt)
        , TArrow (TInt, TArrow (TInt, TInt)) )
    , [ AConst (CInt 5, TInt); AConst (CInt 3, TInt) ]
    , TInt )
;;

(*
   let () =
   _correct_args
   |> part_elim
   |> show_annot_expr
   |> print_endline
*)

let _add42 =
  ALam
    ( [ "x", TInt ]
    , APrim (Add, AVar ("x", TInt), AConst (CInt 42, TInt), TInt)
    , TArrow (TInt, TInt) )
;;

let _applied = AApp (_add42, [ AConst (CInt 42, TInt) ], TInt)
let _add42raw = Lam ([ "x" ], Prim (Add, Var "x", Const (CInt 42)))
let _appraw = App (_add42raw, Const (CInt 42))
let _nested = Lam ([ "x" ], Lam ([ "y" ], Prim (Add, Var "x", Var "y")))

(*
   let _ =
   let (_lifted, _globals) = Lift.lambda_lift_expr [] _add42 in
   (*print_endline (show_annot_expr _lifted)*)
   let head = List.hd _globals in
   let { body; _ } = head in
   print_endline (show_annot_expr body)*)

let _show_tup tup =
  let int, typ = tup in
  print_endline (Printf.sprintf "%s %s" (string_of_int int) (show_typ typ))
;;

let _show_global global =
  let { fundef; _ } = global in
  print_endline ("> " ^ show_annot_expr fundef)
;;

let _a _a =
  (*let (_subst, _annot, _ty) = annotate [] [] _nested in
    (*List.map show_tup _subst |> ignore*)
    print_endline (show_annot_expr _annot)
  *)
  let _subst, _annot, _ty = annotate [] [] _nested in
  let _annot, _globals = Lift.lambda_lift_expr _annot in
  print_endline (show_annot_expr _annot);
  print_endline "separator";
  List.map _show_global _globals |> ignore
;;

let _partialappok =
  ast_of_text "let add = fun x -> fun y -> x + y in let add1 = add 1 in add1 2 "
;;

let _partialappthreesum =
  ast_of_text
    "let add_three = fun x -> fun y -> fun z -> x+y+z in let add1 = add_three 1 in let \
     add2 = add1 2 in add2 3"
;;

let _partialappthreesum2 =
  ast_of_text "let add_three = fun x -> fun y -> fun z -> x+y+z in add_three 3 4 5"
;;

let _simpleadd = ast_of_text "let add = fun x -> fun y -> x+y in add 41 1"
let _toplevel = ast_of_text "def add x y = x + y;"

let _toplevel_eta =
  ast_of_text
    "def add1 y = let add = fun x -> fun y -> x + y in let add1 = add 1 in add1 y;"
;;

(*
   let () =
   Result.map
   (fun annot ->
   print_endline (show_annot_expr annot);
   print_endline "SEP -------";
   print_endline (show_annot_expr (EliminatePartialApp.eliminate_partial annot)))
   _partialappok
   |> ignore
   ;;
*)
(*
   let () =
   Result.map
   (fun annot_exprs ->
   print_endline "---> Pretty print of raw expr";
   List.iter (fun annot -> print_endline (string_of_annot_expr annot)) annot_exprs;
   print_endline "--> Pretty print of eliminated expr";
   (* List.iter
   (fun annot ->
   print_endline
   (string_of_annot_expr (EliminatePartialApp.eliminate_partial annot)))
   annot_exprs;*)
   (* print_endline "--> AST of eliminated expr";
   List.iter
   (fun annot ->
   print_endline (show_annot_expr (EliminatePartialApp.eliminate_partial annot)))
   annot_exprs;*)
   (* print_endline "---> AST of raw expr";
   List.iter (fun annot -> print_endline (show_annot_expr annot)) annot_exprs;*)
   print_endline "---> Lambda lifted exprs";
   List.iter
   (fun annot ->
   let _lifted, _globals = optimize annot in
   print_endline "-----> Lifted entry";
   print_endline (string_of_annot_expr _lifted);
   print_endline "-----> Globals for above entry";
   List.iter
   (fun global -> print_endline (string_of_annot_expr global.fundef))
   _globals)
   annot_exprs)
   _toplevel_eta
   |> ignore
   ;;
*)

(* arg "t" to main is there because we don't support zero arg functions yet *)
let _main_example =
  ast_of_text "def add x y = x+y; def main unused = let x = 41 in let y = 1 in add x y;"
;;

let _simple_main = ast_of_text "def main x y = x+y;"
let _simpler_main = ast_of_text "def main x = x+1;"
let _simpl_main = ast_of_text "def main x = let y = 42 in x+y;"
let _main = ast_of_text "def main = let y = 42 in y;"
let _constant_ret = ast_of_text "def main = let y = 42 in 42;"
let _add1 = ast_of_text "def main = let y = 41 in y+1;"
let _advanced = ast_of_text "def main = let x = 1 in let y = 42 in x+y;"
let _shadow = ast_of_text "def main = let x = 1 in let x = 42 in x;"

(* TODO: this is not handled properly *)
let _twofunctions =
  ast_of_text "def main = let x = 1 in let y = x+1 in let x = 40 in x+y;"
;;

let _function =
  ast_of_text "def add x y = x+y; def main = let x = 40 in let y = 2 in add x y;"
;;

(*
   let () =
   Result.map (fun (subst, _) -> print_endline (string_of_int (List.length subst))) _main
   |> ignore
   ;;
*)
let () =
  Result.map
    (fun annot_exprs ->
      let mapped =
        List.map
          (fun annot ->
            let lifted, _ = optimize annot in
            lifted)
          annot_exprs
      in
      let compiled = init_wat mapped [] in
      print_endline compiled)
      (*
         List.iter
         (fun annot ->
         let _lifted, _ = optimize annot in
         (* no globals in this example *)
         print_endline (string_of_annot_expr _lifted))
         annot_exprs)
      *)
    _twofunctions
  |> ignore
;;
