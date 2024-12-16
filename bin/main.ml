open ComRaTTlib.Preprocess
open ComRaTTlib.Preprocess.ForwardDeclataion
open ComRaTTlib.Ast_of_text
open ComRaTTlib.Infer
open ComRaTTlib.Compile

let ( let* ) = Result.bind

let () =
  (let* exprs = process_stdin () in
   let exprs = infer_all exprs in
   let defs, lifted = optimize_program exprs in
   let nidx, signature = generate_function_tables defs lifted in
   let compiled = init_wat (defs @ lifted) [] nidx signature in
   print_endline compiled;
   Result.ok ())
  |> Result.map_error print_endline
  |> ignore
;;
