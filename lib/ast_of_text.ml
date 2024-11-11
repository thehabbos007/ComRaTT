open Lexing

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
    let token = Lexing.lexeme lexbuf in
    let position = lexbuf.lex_curr_p in
    let line = position.pos_lnum in
    (let column = position.pos_cnum - position.pos_bol + 1 in
     Printf.sprintf
       "Syntax error at line %d, column %d: Unexpected token '%s'\n"
       line
       column
       token)
    |> Result.error
;;

let process_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
  parse_and_print lexbuf
;;

let ( let* ) = Result.bind

let print_global ({ name; fundef; _ } : Preprocess.global_def) =
  Printf.fprintf stdout "%s = %s\n" name (Infer.show_typed_expr fundef) |> ignore
;;

let ast_of_text text =
  (let lexbuf = Lexing.from_string text in
   let* processed = parse_and_print lexbuf in
   let annotated = Infer.infer_all processed in
   Result.ok annotated)
  |> Result.map_error (fun x ->
    print_endline x;
    x)
;;
