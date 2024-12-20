type binop =
  | Add
  | Mul
  | Div
  | Sub
  | Eq
  | Lt
  | Lte
  | Gt
  | Gte
  | Neq
  | Cons
[@@deriving show, eq]

type const =
  | CInt of int
  | CBool of bool
  | CUnit
  | CNil
[@@deriving show, eq]

type expr =
  | Const of const
  | Var of string
  | Lam of string list * expr
  | App of expr * expr
  | Prim of binop * expr * expr
  | Let of string * expr * expr
  | FunDef of string * string list * expr
  | IfThenElse of expr * expr * expr
  | Delay of expr
  | Advance of string
[@@deriving show, eq]

(* Pretty printing *)

let string_of_binop = function
  | Add -> "+"
  | Mul -> "*"
  | Div -> "/"
  | Sub -> "-"
  | Eq -> "="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | Neq -> "<>"
  | Cons -> "::"
;;
