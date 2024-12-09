open ComRaTTlib

let ( let* ) = Result.bind

(* let print_global ({ name; fundef; _ } : Preprocess.global_def) =
  Printf.fprintf stdout "%s = %s\n" name (Infer.show_annot_expr fundef) |> ignore
;;*)

let () =
  (let* processed = Ast_of_text.process_stdin () in
   let annotated = Infer.infer_all processed in
   (* Temporary pop head *)
   let lifted, _globals = Preprocess.optimize [] (List.hd annotated) in
   print_endline "----- Top level functions -----";
   print_endline (Infer.show_typed_expr lifted);
   print_endline "----- Globally lifted lambdas -----";
   (* List.iter print_global globals;*)
   print_endline "----- WAT -----";
   let compiled = Compile.init_wat [ lifted ] [] in
   print_endline compiled;
   Result.ok ())
  |> Result.map_error print_endline
  |> ignore
;;
