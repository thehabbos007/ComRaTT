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

type sym = string [@@deriving show, eq]

type typ =
  | TInt
  | TBool
  | TUnit
  | TVar of typ_var
  (* Refac to TFun of typ list * typ ? *)
  | TArrow of typ * typ
[@@deriving show, eq]

and typ_var = (typ_var_kind * int) ref (* kind and binding level *)

and typ_var_kind =
  | Link of typ
  | Unlink of int
[@@deriving show, eq]

type expr =
  | Const of const
  | Var of string
  | Lam of string list * expr
  | App of expr * expr
  | Prim of binop * expr * expr
  | Let of string * expr * expr
  | FunDef of
      { annotation : typ
      ; name : sym
      ; args : sym list
      ; body : expr
      }
  | IfThenElse of expr * expr * expr
  | Delay of expr
  | Advance of string
[@@deriving show, eq]

type typed_expr =
  | TFunDef of sym * (sym * typ) list * typed_expr * typ
  | TConst of const * typ
  | TName of sym * typ
  | TLam of
      { args : (sym * typ) list
      ; body : typed_expr
      ; typ : typ
      }
  | TApp of
      { fn : typed_expr
      ; args : typed_expr list
      ; typ : typ
      }
  | TPrim of
      { op : binop
      ; left : typed_expr
      ; right : typed_expr
      ; typ : typ
      }
  | TLet of
      { name : sym
      ; typ : typ
      ; rhs : typed_expr
      ; body : typed_expr
      }
  | TIfThenElse of
      { condition : typed_expr
      ; then_branch : typed_expr
      ; else_branch : typed_expr
      ; typ : typ
      }
[@@deriving show, eq]

let rec tarrow_len_n_rec counter ty types n =
  match ty with
  | TArrow (ty, next_ty) when n = 0 -> counter, next_ty, ty :: types
  | TArrow (ty, next_ty) -> tarrow_len_n_rec (counter + 1) next_ty (ty :: types) (n - 1)
  | _ when n > 0 -> failwith "Too many arguments for function"
  | _ -> counter + 1, ty, List.rev types
;;

let tarrow_len_n ty n =
  match ty with
  | TArrow _ when n = 0 -> 0, ty, []
  | TArrow (ty, next_ty) -> tarrow_len_n_rec 1 next_ty [ ty ] n
  | _ -> failwith "Attempted to traverse a non-tarrow type"
;;

let rec infer ctx expr =
  match expr with
  | Var x -> List.assoc_opt x ctx
  | Const c ->
    (match c with
     | CInt _ -> Some TInt
     | CBool _ -> Some TBool
     | CUnit -> Some TUnit)
  | _ -> failwith "Not implemented"

and check ctx expr ty =
  match expr, ty with
  | IfThenElse (cond, thenb, elseb), _ ->
    (match check ctx cond TBool, check ctx thenb ty, check ctx elseb ty with
     | Some TBool, Some _, Some _ -> Some ty
     | _ -> None)
  (*
     A function with arguments. It can call itself so add name to ctx.
     Check length of "signature" against args list. Fail if too many
     arguments are provided. Too few will make the return value a function type
     (to be checked in next step).
     Pop annotation till we reach the return type. check that against body, which
     will initiate infer mode.
  *)
  | FunDef { annotation = TArrow _; name; args; body }, _ ->
    let _, ret_ty, types = tarrow_len_n ty (List.length args) in
    let ctx_addition = (name, ty) :: List.combine args types in
    check (ctx_addition @ ctx) body ret_ty
  (*
     A function with no arguments. Check the (constant) body against the type.
     Dont add name to context as we dont want a recursive constant function.
  *)
  | FunDef { annotation = typ; args = []; body; _ }, _ -> check ctx body typ
  (*
     A non-annotated fundef is illegal.
  *)
  | FunDef _, _ -> None (* Error: non-annotated fundef *)
  (*
     A lambda checked against TArrow.
     unify length of tarrow with length of args
     add args to ctx
     check return type of tarrow against body
     TODO: support nested lambdas? (the len <> List.len args check. could we do like with fundef instead?)
  *)
  | Lam (args, body), TArrow _ ->
    let len, ret_ty, types = tarrow_len_n ty (List.length args) in
    if len <> List.length args
    then None (* Error: lambda args too few/too many *)
    else (
      let ctx_addition = List.combine args types in
      check (ctx_addition @ ctx) body ret_ty)
  | Lam _, _ -> None (* Error: lambda type mismatch *)
  | expr, _ ->
    (match infer ctx expr with
     | Some ty' when ty = ty' -> Some ty
     | _ -> None (* Error: type inference doesn't unify *))
;;

open Base

let%test_unit "test" = [%test_eq: int list] (List.rev [ 1; 2; 3 ]) [ 3; 2; 1 ]
