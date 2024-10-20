%{
open Source
%}

%token <int> INT
%token <string> IDENT
%token LAMBDA LET IN SARROW
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token EQUALS
%token EOF

%start <expr list> prog

%left PLUS MINUS
%left TIMES

%%

prog:
  | e = expr+ EOF { e }

expr:
  | LET x = IDENT args = IDENT* EQUALS e = expr { FunDef (x, args, e) }
  | LET x = IDENT EQUALS e1 = expr IN e2 = expr { Let (x, e1, e2) }
  | LAMBDA x = IDENT* SARROW e = expr { Lam (x, e) }
  | app_expr { $1 }

app_expr:
  | app_expr simple_expr { App ($1, $2) }
  | arith_expr { $1 }

arith_expr:
  | e1 = arith_expr op = binop e2 = arith_expr { Prim (op, e1, e2) }
  | simple_expr { $1 }

simple_expr:
  | i = INT { CstI i }
  | x = IDENT { Var x }
  | LPAREN e = expr RPAREN { e }

%inline binop:
  | PLUS { Add }
  | TIMES { Mul }
  | MINUS { Sub }
