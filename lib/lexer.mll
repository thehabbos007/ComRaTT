{
open Parser
exception SyntaxError of string


}

let line_ending
  = '\r'
  | '\n'
  | "\r\n"
let whitespace =
  [' ' '\t']+

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*

rule skip_whitespace kont = parse
  | line_ending
    { Lexing.new_line lexbuf; (skip_whitespace kont) lexbuf }
  | whitespace
    { skip_whitespace kont lexbuf }
  | ""
    { kont lexbuf }

and token = parse "" { skip_whitespace actual_token lexbuf }

and actual_token = parse
  | "fun"    { LAMBDA }
  | "let"    { LET }
  | "in"     { IN }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "delay"  { DELAY }
  | "advance" { ADVANCE }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "delay"  { DELAY }
  | "advance" { ADVANCE }
  | "int"    { TINT }
  | "bool"   { TBOOL }
  | "()"     { UNIT }
  | "+"      { PLUS }
  | "/"      { DIV }
  | "*"      { TIMES }
  | "-"      { MINUS }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "="      { EQUALS }
  | "->"     { ARROW }
  | ":"      { COLON }
  | ";"      { SEMI }
  | "<"      { LT }
  | "<="     { LTE }
  | ">"      { GT }
  | ">="     { GTE }
  | "<>"     { NEQ }
  | ident as id { IDENT id }
  | digit+ as d { INT (int_of_string d) }
  | eof      { EOF }
  | _  { raise @@ SyntaxError (Lexing.lexeme lexbuf) }
