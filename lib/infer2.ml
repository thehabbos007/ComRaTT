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
  | _ -> counter, ty, List.rev types
;;

let tarrow_len_n ty n =
  match ty with
  | TArrow _ when n = 0 -> 0, ty, []
  | TArrow (ty, next_ty) -> tarrow_len_n_rec 1 next_ty [ ty ] (n - 1)
  | _ -> failwith "Attempted to traverse a non-tarrow type"
;;

let rec infer ctx expr =
  match expr with
  | Let (name, rhs, body) ->
    (match infer ctx rhs with
     | Some rhs_ty ->
       (match infer ((name, rhs_ty) :: ctx) body with
        | Some body_ty -> Some body_ty
        | None -> None)
     | None -> None)
  | Prim (op, e1, e2) ->
    (match op with
     (*
        For arith operators we want to check both sides against int (which defaults to infer).
      Given two Somes, everything is good and TInt is the result.
     *)
     | Add | Mul | Div | Sub ->
       (match check ctx e1 TInt, check ctx e2 TInt with
        | Some _, Some _ -> Some TInt
        | _ -> None)
     (*
        For non eq comparison operators we want to check both sides against int (which defaults to infer).
      Given two Somes, everything is good and TBool is the result.
     *)
     | Lt | Lte | Gt | Gte ->
       (match check ctx e1 TInt, check ctx e2 TInt with
        | Some _, Some _ -> Some TBool
        | _ -> None)
     (*
        For eq and neq there are two valid cases: both operands are TInt or both are TBool.
      In either case, the result is TBool.
      For that reason, we infer directly and check equality.
     *)
     | Eq | Neq ->
       (match infer ctx e1, infer ctx e2 with
        | Some TInt, Some TInt -> Some TBool
        | Some TBool, Some TBool -> Some TBool
        | _ -> None))
  | App (fn, arg) ->
    (* Infer the type of the function, which should be known.
    That type will be an arrow type giving us something to
    check the argument against, by popping the first type of the TArrow.
    If the function is somehow not a TArrow, that is a failure.
    If check returns some, then we we are good.
    *)
    (match infer ctx fn with
     | Some (TArrow (ty, _)) ->
       (match check ctx arg ty with
        | Some ty' -> Some ty' (* The same as ty *)
        | None -> None)
     | Some _ -> failwith "Type of function was not TArrow"
     | None -> None)
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
  If all that goes well, return the functions type as given by the annotation.
  Misuse of arguments is handled when checking the body.
  *)
  | FunDef { annotation = TArrow _ as fun_type; name; args; body }, _ ->
    let _, ret_ty, types = tarrow_len_n ty (List.length args) in
    let ctx_addition = (name, ty) :: List.combine args types in
    (match check (ctx_addition @ ctx) body ret_ty with
     | Some _ -> Some fun_type
     | None -> None)
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

(*
   TODO
  It feels wierd that the function-related tests just assert on the
  type from the annotation (which is also returned by check).
  The output should be None if the type checking fails, so a Some case
  should always be correct. Using "assert true" could also work.

  Also, tests will be more precise when we have refactored to use clear error types instead of
  None for everything.
*)

let%test_unit "Check conditional expression" =
  let conditional = IfThenElse (Const (CBool true), Const (CInt 42), Const (CInt 0)) in
  match check [] conditional TInt with
  | Some ty -> OUnit2.assert_equal ~printer:show_typ TInt ty
  | None -> OUnit2.assert_bool "Failed to infer let binding" false
;;

let%test_unit
    "Check conditional expression should fail when branches have different types"
  =
  let conditional =
    IfThenElse (Const (CBool true), Const (CInt 42), Const (CBool false))
  in
  match check [] conditional TInt with
  | Some _ ->
    OUnit2.assert_bool
      "Checking conditional with different branch types should fail"
      false
  | None ->
    OUnit2.assert_bool
      "Correctly failed to check conditional with different branch types"
      true
;;

let%test_unit "Infer let-binding: let x = 2 in x" =
  let binding = Let ("x", Const (CInt 2), Var "x") in
  let checked_binding = infer [] binding in
  match checked_binding with
  | Some ty -> OUnit2.assert_equal ~printer:show_typ TInt ty
  | None -> OUnit2.assert_bool "Failed to infer let binding" false
;;

let%test_unit "Infer let-binding: let x = 2 in x+x" =
  let binding = Let ("x", Const (CInt 2), Prim (Add, Var "x", Var "x")) in
  let checked_binding = infer [] binding in
  match checked_binding with
  | Some ty -> OUnit2.assert_equal ~printer:show_typ TInt ty
  | None -> OUnit2.assert_bool "Failed to infer let binding" false
;;

let%test_unit "Function with let-binding" =
  let fn_type = TArrow (TInt, TBool) in
  let fn_args = [ "x" ] in
  let fn_body = Let ("y", Const (CInt 2), Prim (Eq, Var "x", Var "y")) in
  let fn =
    FunDef { annotation = fn_type; name = "test"; args = fn_args; body = fn_body }
  in
  match check [] fn fn_type with
  | Some ty -> OUnit2.assert_equal ~printer:show_typ fn_type ty
  | None -> OUnit2.assert_bool "Failed to check function with let binding" false
;;

let%test_unit "Function with incorrect signature fails type checking" =
  let fn_type = TArrow (TInt, TBool) in
  let fn_args = [ "x" ] in
  let fn_body = Let ("y", Const (CInt 2), Prim (Add, Var "x", Var "y")) in
  let fn =
    FunDef { annotation = fn_type; name = "test"; args = fn_args; body = fn_body }
  in
  match check [] fn fn_type with
  | Some _ ->
    OUnit2.assert_bool
      "Type checking a function with incorrect signature should fail!"
      false
  | None ->
    OUnit2.assert_bool "Correctly failed to check function with incorrect signature" true
;;

(*
   Type: int
def test = 2
*)
let%test_unit "Checking constant function should return correct type" =
  let fn_type = TInt in
  let fn =
    FunDef { annotation = fn_type; name = "test"; args = []; body = Const (CInt 2) }
  in
  let checked_type = check [] fn fn_type in
  match checked_type with
  | Some ty -> OUnit2.assert_equal TInt ty
  | None -> OUnit2.assert_bool "Failed to check function" false
;;

(*
   Type int -> int
def test x = x+2
*)
let%test_unit "Check function that adds variable to constant should return correct type" =
  let fn_type = TArrow (TInt, TInt) in
  let fn =
    FunDef
      { annotation = fn_type
      ; name = "test"
      ; args = [ "x" ]
      ; body = Prim (Add, Var "x", Const (CInt 2))
      }
  in
  let checked_type = check [] fn fn_type in
  match checked_type with
  | Some ty -> OUnit2.assert_equal ~printer:show_typ (TArrow (TInt, TInt)) ty
  | None -> failwith "failed to check"
;;

let%test_unit "tarrow_len_n given non TArrow type raises Failure" =
  OUnit2.assert_raises (Failure "Attempted to traverse a non-tarrow type") (fun () ->
    tarrow_len_n TBool 1)
;;

let%test_unit
    "tarrow_len_n given TArrow type and n larger than amount of arguments raises Failure"
  =
  OUnit2.assert_raises (Failure "Too many arguments for function") (fun () ->
    tarrow_len_n (TArrow (TInt, TArrow (TInt, TBool))) 3)
;;

let%test_unit "tarrow_len_n given TArrow and n=0 returns type unmodified" =
  let arrow = TArrow (TInt, TBool) in
  let counter, ret_ty, types = tarrow_len_n arrow 0 in
  OUnit2.assert_equal 0 counter;
  OUnit2.assert_equal arrow ret_ty;
  OUnit2.assert_equal [] types
;;

(* int -> bool *)
let%test_unit "tarrow_len_n given TArrow and n = args length returns correct result" =
  let arrow = TArrow (TInt, TBool) in
  let counter, ret_ty, types = tarrow_len_n arrow 1 in
  OUnit2.assert_equal 1 counter;
  OUnit2.assert_equal TBool ret_ty;
  OUnit2.assert_equal [ TInt ] types
;;

(* int -> int -> bool *)
let%test_unit "tarrow_len_n given TArrow and n = args length returns correct result" =
  let arrow = TArrow (TInt, TArrow (TInt, TBool)) in
  let counter, ret_ty, types = tarrow_len_n arrow 2 in
  OUnit2.assert_equal 2 counter;
  OUnit2.assert_equal TBool ret_ty;
  OUnit2.assert_equal [ TInt; TInt ] types
;;
