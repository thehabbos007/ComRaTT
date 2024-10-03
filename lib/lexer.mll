{
open Parser
exception LexError of string
}

let white = [' ' '\t' '\n']+
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*

rule token = parse
  | white    { token lexbuf }
  | "fun" { LAMBDA }
  | "let"    { LET }
  | "in"     { IN }
  | "+"      { PLUS }
  | "*"      { TIMES }
  | "-"      { MINUS }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "="      { EQUALS }
  | ident as id { IDENT id }
  | digit+ as d { INT (int_of_string d) }
  | eof      { EOF }
  | _ as c   { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }
