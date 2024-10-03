open Lexing
open ComRaTTlib
(* open ComRaTTlib.Lexer

   let prog = "prog.wat" *)

let pos_string pos =
  let open Lexing in
  Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_and_print lexbuf =
  try
    let ast = Parser.prog Lexer.token lexbuf in
    Result.ok ast
  with
  | Lexer.LexError msg ->
    (let pos = lexbuf.lex_curr_p in
     Printf.sprintf "Lexing error at %s: %s\n" (pos_string pos) msg)
    |> Result.error
  | Parser.Error ->
    (let pos = lexbuf.lex_curr_p in
     Printf.sprintf "Parsing error at %s\n" (pos_string pos))
    |> Result.error
;;

let process_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
  parse_and_print lexbuf
;;

let () =
  let processed = process_stdin () in
  Result.map (Annotate.annotate [] []) processed
  |> Result.map (fun (_, ast, _) -> ast)
  |> Result.map (Preprocess.lambda_lift_expr [])
  |> Result.map (fun (lifted, _globals) -> Annotate.show_annot_expr lifted)
  |> Result.map print_endline
  |> Result.map_error print_endline
  |> ignore
;;
