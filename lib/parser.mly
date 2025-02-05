%{
open Source
%}

%token <int> INT
%token <string> IDENT
%token LAMBDA IN LET SEMI IF THEN ELSE ADVANCE DELAY
%token PLUS TIMES MINUS DIV TRUE FALSE UNIT
%token LT LTE GT GTE NEQ COLON ARROW
%token LPAREN RPAREN EQUALS EOF
%token TINT TBOOL TUNIT

%start <prog> prog

%left LT LTE GT GTE NEQ
%left PLUS MINUS
%left EQUALS
%left TIMES DIV

%%

prog:
  | fs = fundef* EOF { fs }

fundef:
  | name = IDENT COLON t = typ LET IDENT args = optargs EQUALS e = expr SEMI
    {
        FunDef (name, t, args, e)
    }

optargs:
  | { [] }
  | args = IDENT+ { args }

typ:
  | t = arrow_typ { t }

arrow_typ:
  | t1 = atomic_typ ARROW t2 = arrow_typ { TFun (t1, t2) }
  | t = atomic_typ { t }

atomic_typ:
  | TINT { TInt }
  | TBOOL { TBool }
  | TUNIT { TUnit }
  | LPAREN t = typ RPAREN { t }
  | id = IDENT { TVar id }

expr:
  | LET x = IDENT EQUALS e1 = expr IN e2 = expr { Let (x, e1, e2) }
  | LAMBDA xs = IDENT* ARROW e = expr { Lam (xs, e) }
  | app_expr { $1 }
  | IF guard = app_expr THEN then_branch = app_expr ELSE else_branch = app_expr
    { IfThenElse(guard, then_branch, else_branch) }

app_expr:
  | DELAY e = simple_expr { Delay e }
  | ADVANCE x = IDENT { Advance x }
  | app_expr simple_expr { App ($1, $2) }
  | arith_expr { $1 }

arith_expr:
  | e1 = arith_expr op = binop e2 = arith_expr { Prim (op, e1, e2) }
  | e1 = arith_expr comp = compare e2 = arith_expr { Prim (comp, e1, e2) }
  | simple_expr { $1 }

simple_expr:
  | x = IDENT { Var x }
  | i = INT { Const (CInt i) }
  | b = bool { Const b }
  | UNIT { Const CUnit }
  | LPAREN e = expr RPAREN { e }

bool:
  | TRUE { CBool true }
  | FALSE { CBool false }

%inline binop:
  | PLUS { Add }
  | TIMES { Mul }
  | DIV { Div }
  | MINUS { Sub }

%inline compare:
  | EQUALS { Eq }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | NEQ { Neq }
