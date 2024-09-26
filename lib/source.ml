type binop =
  | Add
  | Mul
  | Sub

type expr =
  | CstI of int
  | Var of string
  | Lam of string * expr
  | App of expr * expr
  | Prim of binop * expr * expr
  | Let of string * expr * expr

(* Pretty printing *)

let string_of_binop = function
  | Add -> "+"
  | Mul -> "*"
  | Sub -> "-"
;;

let rec string_of_expr = function
  | CstI i -> string_of_int i
  | Var x -> x
  | Lam (x, e) -> "λ" ^ x ^ "." ^ string_of_expr e
  | App (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | Prim (op, e1, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
;;
