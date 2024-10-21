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
    (let pos = lexbuf.lex_curr_p in
     Printf.sprintf "Parsing error at %s\n" (pos_string pos))
    |> Result.error
;;

let process_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
  parse_and_print lexbuf
;;

let ( let* ) = Result.bind

let print_global ({ name; body; _ } : Preprocess.global_def) =
  Printf.fprintf stdout "%s = %s\n" name (Annotate.show_annot_expr body) |> ignore
;;

let ast_of_text text =
  (let lexbuf = Lexing.from_string text in
   let* processed =
     lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "stdin" };
     parse_and_print lexbuf
   in
   let _, annotated, _ = Annotate.annotate [] [] processed in
   (* let lifted, globals = Preprocess.optimize annotated in*)
   Result.ok annotated)
  |> Result.map_error (fun x ->
    print_endline x;
    x)
;;
