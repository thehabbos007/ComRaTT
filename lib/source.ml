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
[@@deriving show, eq]

type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TVar of string
[@@deriving show, eq]

type expr =
  | Const of const
  | Var of string
  | Lam of string list * expr
  | App of expr * expr
  | Prim of binop * expr * expr
  | Let of string * expr * expr
  | IfThenElse of expr * expr * expr
  | Delay of expr
  | Advance of string
[@@deriving show, eq]

type toplevel = FunDef of string * typ * string list * expr [@@deriving show, eq]
type prog = toplevel list [@@deriving show, eq]
