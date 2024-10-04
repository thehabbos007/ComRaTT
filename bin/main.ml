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

let ( let* ) = Result.bind

let print_global ({ name; body; _ } : Preprocess.global_def) =
  Printf.fprintf stdout "%s = %s\n" name (Annotate.show_annot_expr body) |> ignore
;;

let () =
  (let* processed = process_stdin () in
   let _, annotated, _ = Annotate.annotate [] [] processed in
   let lifted, globals = Preprocess.optimize annotated in
   print_endline "----- Top level functions -----";
   print_endline (Annotate.show_annot_expr lifted);
   print_endline "----- Globally lifted lambdas -----";
   List.iter print_global globals;
   print_endline "----- WAT -----";
   let compiled = Compile.init_wat lifted globals in
   print_endline compiled;
   Result.ok ())
  |> Result.map_error print_endline
  |> ignore
;;
