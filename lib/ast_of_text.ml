module P = Parser
module I = Parser.MenhirInterpreter
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil

module Message = struct
  type t = Parsing_error

  let default_severity _ = Asai.Diagnostic.Error

  let short_code : t -> string = function
    | Parsing_error -> "parsing-error"
  ;;
end

module Reporter = Asai.Reporter.Make (Message)
module Term = Asai.Tty.Make (Message)

let display_diagnostic = Term.display
let succeed x = Ok x

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> failwith "Can only get env from HandlingError checkpoint"
;;

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None -> 0
;;

let fail lexbuf _buffer (checkpoint : _ I.checkpoint) =
  let message = Parser_messages.message (state checkpoint) in
  Reporter.fatalf
    ~loc:(Asai.Range.of_lexbuf lexbuf)
    Parsing_error
    "%S"
    message
    (Printf.sprintf "%s%s%s%!" message)
;;

let try_parse filepath =
  Reporter.run ~emit:display_diagnostic ~fatal:(fun err -> Error err)
  @@ fun () ->
  let lexbuf = Lexing.from_channel (open_in filepath) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filepath };
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.prog lexbuf.lex_curr_p in
  I.loop_handle succeed (fail lexbuf buffer) supplier checkpoint
;;

let parse_from_file = try_parse
