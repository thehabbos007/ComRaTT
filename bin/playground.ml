open ComRaTTlib.Preprocess
open ComRaTTlib.Ast_of_text
open ComRaTTlib.Compile

let _function = ast_of_text "def main = let x = fun x -> x + 1 in x 1;"

let () =
  Result.map
    (fun annot_exprs ->
      let defs, lifted = optimize_program annot_exprs in
      let compiled = init_wat (defs @ lifted) [] in
      print_endline compiled)
      (*
         List.iter
         (fun annot ->
         let _lifted, _ = optimize annot in
         (* no globals in this example *)
         print_endline (string_of_annot_expr _lifted))
         annot_exprs)
      *)
    _function
  |> ignore
;;
