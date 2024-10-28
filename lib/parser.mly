%{
open Source
%}

%token <int> INT
%token <string> IDENT
%token LAMBDA IN LET DEF SARROW SEMI IF THEN ELSE
%token PLUS TIMES MINUS TRUE FALSE UNIT
%token LT LTE GT GTE NEQ
%token LPAREN RPAREN
%token EQUALS
%token EOF

%start <expr list> prog

%left LT LTE GT GTE NEQ
%left PLUS MINUS
%left EQUALS
%left TIMES

%%

let optarg(x) :=
  | { [] }
  | x=x ; { [x] }

prog:
  | fs = fundefs* EOF { fs }

optargs:
    | { [] }
    | args = IDENT+ { args }

fundefs:
  | DEF x = IDENT args = optargs EQUALS e = expr SEMI { FunDef (x, args, e) }

expr:
  | LET x = IDENT EQUALS e1 = expr IN e2 = expr { Let (x, e1, e2) }
  | LAMBDA x = IDENT* SARROW e = expr { Lam (x, e) }
  | app_expr { $1 }
  | IF guard = app_expr THEN then_branch = app_expr ELSE else_branch = app_expr { IfThenElse(guard, then_branch, else_branch) }

app_expr:
  | app_expr simple_expr { App ($1, $2) }
  | arith_expr { $1 }

arith_expr:
  | e1 = arith_expr op = binop e2 = arith_expr { Prim (op, e1, e2) }
  | e1 = arith_expr comp = compare e2 = arith_expr { Prim (comp, e1, e2) }
  | simple_expr { $1 }

simple_expr:
  | x = IDENT { Var x }
  | i = INT { Const (CInt i) }
  | b = bool { Const (b) }
  | UNIT { Const CUnit }
  | LPAREN e = expr RPAREN { e }

bool:
  | TRUE { CBool true }
  | FALSE { CBool false }

%inline binop:
  | PLUS { Add }
  | TIMES { Mul }
  | MINUS { Sub }

%inline compare:
    | EQUALS { Eq }
    | LT { Lt }
    | LTE { Lte }
    | GT { Gt }
    | GTE { Gte }
    | NEQ { Neq }
