open ComRaTTlib.Preprocess
open ComRaTTlib.Preprocess.ForwardDeclataion
open ComRaTTlib.Infer
open ComRaTTlib.Compile

let ast_of_text _s = failwith ""
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
let _leq = ast_of_text "def main = let x = 5 < 2 in x;"

let _twofunctions =
  ast_of_text "def main = let x = 1 in let y = x+1 in let x = 40 in x+y;"
;;

let _function =
  ast_of_text "def add x y = x+y; def main = let x = 40 in let y = 2 in add x y;"
;;

let _delay_adv =
  ast_of_text "def add x y = x + y; def main = let x = delay (add 2 3) in advance x;"
;;

let _closures =
  ast_of_text
    "def add x y = x + y; def main = let x = 4 in let b = delay (add x 3) in advance b;"
;;

(*
   f x =
   let dingo z = z + x
   in let bingo w = dingo (47 - w)
   in g (bingo 419)

   g u =
   let mango x = x + h u
   in let dingo k = k
   in mango (dingo u)

   h w = 42
*)
let _frub = ast_of_text "def fib x = if x = 2 then 8 else fib (x + 1); def main = fib 2;"

(*
   let () =
   Result.map (fun (subst, _) -> print_endline (string_of_int (List.length subst))) _main
   |> ignore
   ;;
*)

let _show_funtable table =
  FunTable.iter
    (fun idx (args, typ) ->
       print_endline
         ("Function with index "
          ^ string_of_int idx
          ^ " and type: "
          ^ string_of_type typ
          ^ " has args: ");
       List.iter
         (fun (name, ty) ->
            print_endline ("  arg_name: " ^ name ^ " ty: " ^ string_of_type ty))
         args)
    table
;;

let () =
  Result.map
    (fun annot_exprs ->
       let defs, lifted = optimize_program annot_exprs in
       let nidx, signature = generate_function_tables defs lifted in
       let compiled = init_wat (defs @ lifted) [] nidx signature in
       print_endline compiled)
    (* let compiled = init_wat (defs @ lifted) [] in
      print_endline compiled)*)
    (*
       List.iter
       (fun annot ->
       let _lifted, _ = optimize annot in
       (* no globals in this example *)
       print_endline (string_of_annot_expr _lifted))
       annot_exprs)
    *)
    (* (ast_of_text "def main = let x = delay 42 in advance x;")*)
    (* (ast_of_text
       "def add x y = x+y; def main = let x = 40 in let y = 2 in let delayed = delay \
       (add x y) in  advance delayed;")*)
    (* (ast_of_text "def add x y = x+y; def main = let x = 40 in let y = 2 in add x y;")*)
    (* (ast_of_text "def add x y = x+y; def main = let x = 40 in let y = 2 in add x y;")*)
    (* (ast_of_text "def main = let hanzo = fun x y -> x+y in hanzo 40 2;")*)
    _closures
  |> ignore
;;
