open ComRaTTlib.Preprocess
open ComRaTTlib.Ast_of_text
open ComRaTTlib.Compile

let _function = ast_of_text "def main = let x = fun x -> x + 1 in x 1;"

let () =
  Result.map
    (fun annot_exprs ->
      let mapped =
        List.map
          (fun expr ->
            let l, gs = optimize expr in
            l :: gs)
          annot_exprs
        |> List.concat
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
    _function
  |> ignore
;;
