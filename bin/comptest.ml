open ComRaTTlib.Preprocess
open ComRaTTlib.Ast_of_text
open ComRaTTlib.Compile

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
    (ast_of_text "def main = let x = delay 42 in advance x;")
  |> ignore
;;
