type binop =
  | Add
  | Mul
  | Sub
[@@deriving show, eq]

type expr =
  | CstI of int
  | Var of string
  | Lam of string list * expr
  | App of expr * expr
  | Prim of binop * expr * expr
  | Let of string * expr * expr
  | FunDef of string * string list * expr
[@@deriving show, eq]

(* Pretty printing *)

let string_of_binop = function
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
;;
