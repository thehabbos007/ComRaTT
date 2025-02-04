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

let rec build_lambda_type args ret_ty =
  match args with
  | [] -> ret_ty
  | ty :: tys -> TArrow (ty, build_lambda_type tys ret_ty)
;;

let tarrow_len_n ty n =
  match ty with
  | TArrow _ when n = 0 -> 0, ty, []
  | TArrow (ty, next_ty) -> tarrow_len_n_rec 1 next_ty [ ty ] (n - 1)
  | _ -> failwith "Attempted to traverse a non-tarrow type"
;;

let rec infer ctx expr : (typ * typed_expr) option =
  match expr with
  (*
     For Advance, look up the type of the name being advanced
    which should result in a thunk. Produce the return type of the thunk.
    Fail if not a thunk.
  *)
  | Advance name ->
    (match List.assoc_opt name ctx with
     | Some (TArrow (TUnit, ty)) ->
       Some
         ( ty
         , TApp
             { fn = TName (name, TArrow (TUnit, ty))
             ; args = [ TConst (CUnit, TUnit) ]
             ; typ = ty
             } )
     | Some _ -> None
     | None -> None)
  (*
     For Delay, infer the type of the expression and
    produce a thunk with that type: () -> ty
  *)
  | Delay e ->
    (match infer ctx e with
     | Some (ty, texp) ->
       Some
         ( TArrow (TUnit, ty)
         , TLam
             { args = [ "#advance_unit", TUnit ]; body = texp; typ = TArrow (TUnit, ty) }
         )
     | None -> None)
  | Let (name, rhs, body) ->
    (match infer ctx rhs with
     | Some (rhs_ty, rhs_texp) ->
       (match infer ((name, rhs_ty) :: ctx) body with
        | Some (body_ty, body_texp) ->
          Some (body_ty, TLet { name; typ = body_ty; rhs = rhs_texp; body = body_texp })
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
        | Some (_, te1), Some (_, te2) ->
          Some (TInt, TPrim { op; left = te1; right = te2; typ = TInt })
        | _ -> None)
     (*
        For non eq comparison operators we want to check both sides against int (which defaults to infer).
      Given two Somes, everything is good and TBool is the result.
     *)
     | Lt | Lte | Gt | Gte ->
       (match check ctx e1 TInt, check ctx e2 TInt with
        | Some (_, te1), Some (_, te2) ->
          Some (TBool, TPrim { op; left = te1; right = te2; typ = TBool })
        | _ -> None)
     (*
        For eq and neq there are two valid cases: both operands are TInt or both are TBool.
      In either case, the result is TBool.
      For that reason, we infer directly and check equality.
     *)
     | Eq | Neq ->
       (match infer ctx e1, infer ctx e2 with
        | Some (TInt, te1), Some (TInt, te2) ->
          Some (TBool, TPrim { op; typ = TBool; left = te1; right = te2 })
        | Some (TBool, te1), Some (TBool, te2) ->
          Some (TBool, TPrim { op; typ = TBool; left = te1; right = te2 })
        | _ -> None))
  | App (fn, arg) ->
    (* Infer the type of the function, which should be known.
    That type will be an arrow type giving us something to
    check the argument against, by popping the first type of the TArrow.
    If the function is somehow not a TArrow, that is a failure.
    If check returns some, then we we are good.
    *)
    (match infer ctx fn with
     | Some (TArrow (ty, _), fn_texp) ->
       (match check ctx arg ty with
        | Some (ty', arg_texp) ->
          Some (ty', TApp { args = [ arg_texp ]; typ = ty'; fn = fn_texp })
          (* ty' is the same as ty *)
        | None -> None)
     | Some _ -> failwith "Type of function was not TArrow"
     | None -> None)
  | Var x ->
    (match List.assoc_opt x ctx with
     | Some ty -> Some (ty, TName (x, ty))
     | None -> None)
  | Const c ->
    (match c with
     | CInt _ -> Some (TInt, TConst (c, TInt))
     | CBool _ -> Some (TBool, TConst (c, TBool))
     | CUnit -> Some (TUnit, TConst (c, TUnit)))
  | _ -> failwith "Not implemented"

and check ctx expr ty : (typ * typed_expr) option =
  match expr, ty with
  | IfThenElse (cond, thenb, elseb), _ ->
    (match check ctx cond TBool, check ctx thenb ty, check ctx elseb ty with
     | Some (TBool, tcond), Some (_, tthen), Some (_, telse) ->
       Some
         ( ty
         , TIfThenElse
             { condition = tcond; typ = ty; then_branch = tthen; else_branch = telse } )
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
    let args_with_types = List.combine args types in
    let ctx_addition = (name, ty) :: args_with_types in
    (match check (ctx_addition @ ctx) body ret_ty with
     | Some (ty, typed_fun) ->
       Some (fun_type, TFunDef (name, args_with_types, typed_fun, ty))
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
  if that goes well, build the type of the lambda from the arguments and the return.
  TODO: support nested lambdas? (the len <> List.len args check. could we do like with fundef instead?)
  *)
  | Lam (args, body), TArrow _ ->
    let len, ret_ty, types = tarrow_len_n ty (List.length args) in
    if len <> List.length args
    then None (* Error: lambda args too few/too many *)
    else (
      let ctx_addition = List.combine args types in
      match check (ctx_addition @ ctx) body ret_ty with
      | Some (_, texp) ->
        let lambda_type = build_lambda_type types ret_ty in
        Some (lambda_type, TLam { args = ctx_addition; body = texp; typ = lambda_type })
      | None -> None)
  | Lam _, _ -> None (* Error: lambda type mismatch *)
  | expr, _ ->
    (match infer ctx expr with
     | Some (ty', texp) when ty = ty' -> Some (ty, texp)
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

  Also, the failure strings used in assert_bool with true constant are redundant.
*)

let%test_unit "build_lambda_type returns correct type" =
  let expected_type = TArrow (TInt, TArrow (TBool, TBool)) in
  let arg_types = [ TInt; TBool ] in
  let ret_ty = TBool in
  let actual_type = build_lambda_type arg_types ret_ty in
  OUnit2.assert_equal expected_type actual_type
;;

let%test_unit "Valid function with shadowing should type check correctly" =
  let fn_type = TArrow (TInt, TInt) in
  let fn_args = [ "x" ] in
  let fn_body = Let ("x", Const (CInt 40), Prim (Add, Var "x", Const (CInt 2))) in
  let fn =
    FunDef { annotation = fn_type; name = "test"; args = fn_args; body = fn_body }
  in
  match check [] fn fn_type with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ fn_type ty;
    OUnit2.assert_equal
      (TFunDef
         ( "test"
         , [ "x", TInt ]
         , TLet
             { name = "x"
             ; typ = TInt
             ; rhs = TConst (CInt 40, TInt)
             ; body =
                 TPrim
                   { op = Add
                   ; left = TName ("x", TInt)
                   ; right = TConst (CInt 2, TInt)
                   ; typ = TInt
                   }
             }
         , TInt ))
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_bool "Failed to check valid function with shadowing" false
;;

let%test_unit
    "Valid function with shadowing where types change should type check correctly"
  =
  let fn_type = TArrow (TInt, TBool) in
  let fn_args = [ "x" ] in
  let fn_body = Let ("x", Const (CBool true), Var "x") in
  let fn =
    FunDef { annotation = fn_type; name = "test"; args = fn_args; body = fn_body }
  in
  match check [] fn fn_type with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ fn_type ty;
    OUnit2.assert_equal
      (TFunDef
         ( "test"
         , [ "x", TInt ]
         , TLet
             { name = "x"
             ; typ = TBool
             ; rhs = TConst (CBool true, TBool)
             ; body = TName ("x", TBool)
             }
         , TBool ))
      texp
      ~printer:show_typed_expr
  | None ->
    OUnit2.assert_bool
      "Failed to check valid function with shadowing where types change"
      false
;;

let%test_unit "Complex, valid function should type check correctly" =
  let fn_type = TArrow (TInt, TInt) in
  let fn_args = [ "x" ] in
  let fn_body =
    IfThenElse
      ( Prim (Eq, Var "x", Const (CInt 2))
      , Let ("del", Delay (Prim (Mul, Var "x", Const (CInt 2))), Advance "del")
      , Prim (Add, Var "x", Const (CInt 40)) )
  in
  let fn =
    FunDef { annotation = fn_type; name = "test"; args = fn_args; body = fn_body }
  in
  match check [] fn fn_type with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ fn_type ty;
    OUnit2.assert_equal
      (TFunDef
         ( "test"
         , [ "x", TInt ]
         , TIfThenElse
             { condition =
                 TPrim
                   { op = Eq
                   ; left = TName ("x", TInt)
                   ; right = TConst (CInt 2, TInt)
                   ; typ = TBool
                   }
             ; then_branch =
                 TLet
                   { name = "del"
                   ; typ = TInt
                   ; rhs =
                       TLam
                         { args = [ "#advance_unit", TUnit ]
                         ; body =
                             TPrim
                               { op = Mul
                               ; left = TName ("x", TInt)
                               ; right = TConst (CInt 2, TInt)
                               ; typ = TInt
                               }
                         ; typ = TArrow (TUnit, TInt)
                         }
                   ; body =
                       TApp
                         { fn = TName ("del", TArrow (TUnit, TInt))
                         ; args = [ TConst (CUnit, TUnit) ]
                         ; typ = TInt
                         }
                   }
             ; else_branch =
                 TPrim
                   { op = Add
                   ; left = TName ("x", TInt)
                   ; right = TConst (CInt 40, TInt)
                   ; typ = TInt
                   }
             ; typ = TInt
             }
         , TInt ))
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_bool "Failed to check complex, valid function" false
;;

let%test_unit "Checking a lambda against non-TArrow type should fail" =
  let lam = Lam ([ "x" ], Const (CInt 2)) in
  match check [] lam TInt with
  | Some _ ->
    OUnit2.assert_bool
      "Should have failed to check type of lambda against non-TArrow"
      false
  | None ->
    OUnit2.assert_bool "Correctly failed to check type of lambda against non-TArrow" true
;;

let%test_unit "Checking a valid lambda should not fail" =
  let lam = Lam ([ "x" ], Prim (Add, Var "x", Const (CInt 2))) in
  match check [] lam (TArrow (TInt, TInt)) with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ (TArrow (TInt, TInt)) ty;
    OUnit2.assert_equal
      (TLam
         { args = [ "x", TInt ]
         ; body =
             TPrim
               { op = Add
               ; left = TName ("x", TInt)
               ; right = TConst (CInt 2, TInt)
               ; typ = TInt
               }
         ; typ = TArrow (TInt, TInt)
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_bool "Failed to check valid lambda" false
;;

let%test_unit "Invalid application should fail type checking" =
  let app = App (Var "f", Const (CBool true)) in
  match infer [ "f", TArrow (TInt, TInt) ] app with
  | Some _ ->
    OUnit2.assert_bool "Should have failed to infer type of invalid application" false
  | None ->
    OUnit2.assert_bool "Correctly failed to infer type of invalid application" true
;;

let%test_unit "Valid application should type check correctly" =
  let app = App (Var "f", Const (CInt 2)) in
  match infer [ "f", TArrow (TInt, TInt) ] app with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal
      (TApp
         { fn = TName ("f", TArrow (TInt, TInt))
         ; args = [ TConst (CInt 2, TInt) ]
         ; typ = TInt
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_bool "Failed to check valid application" false
;;

let%test_unit "Infer Delay should produce a thunk" =
  let delayed = Delay (Const (CInt 2)) in
  match infer [] delayed with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ (TArrow (TUnit, TInt)) ty;
    OUnit2.assert_equal
      (TLam
         { args = [ "#advance_unit", TUnit ]
         ; body = TConst (CInt 2, TInt)
         ; typ = TArrow (TUnit, TInt)
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_bool "Failed to infer type of delay" false
;;

let%test_unit "Infer Advance should not fail when name is bound to a thunk" =
  let adv = Advance "x" in
  match infer [ "x", TArrow (TUnit, TInt) ] adv with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal
      (TApp
         { fn = TName ("x", TArrow (TUnit, TInt))
         ; args = [ TConst (CUnit, TUnit) ]
         ; typ = TInt
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_bool "Failed to infer type of advance" false
;;

let%test_unit "Infer Advance should fail when name is not bound to a thunk" =
  let adv = Advance "x" in
  match infer [ "x", TArrow (TInt, TInt) ] adv with
  | Some _ ->
    OUnit2.assert_bool
      "Should have failed to infer type of advance on name bound to non-thunk"
      false
  | None ->
    OUnit2.assert_bool
      "Correctly failed to infer type of advance on name bound to non-thunk"
      true
;;

let%test_unit "Infer Advance should fail when name is not bound" =
  let adv = Advance "x" in
  match infer [] adv with
  | Some _ ->
    OUnit2.assert_bool
      "Should have failed to infer type of advance on name not bound"
      false
  | None ->
    OUnit2.assert_bool "Correctly failed to infer type of advance on name not bound" true
;;

let%test_unit "Check conditional expression" =
  let conditional = IfThenElse (Const (CBool true), Const (CInt 42), Const (CInt 0)) in
  match check [] conditional TInt with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal
      (TIfThenElse
         { condition = TConst (CBool true, TBool)
         ; then_branch = TConst (CInt 42, TInt)
         ; else_branch = TConst (CInt 0, TInt)
         ; typ = TInt
         })
      texp
      ~printer:show_typed_expr
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
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal
      (TLet
         { name = "x"; typ = TInt; rhs = TConst (CInt 2, TInt); body = TName ("x", TInt) })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_bool "Failed to infer let binding" false
;;

let%test_unit "Infer let-binding: let x = 2 in x+x" =
  let binding = Let ("x", Const (CInt 2), Prim (Add, Var "x", Var "x")) in
  let checked_binding = infer [] binding in
  match checked_binding with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal
      (TLet
         { name = "x"
         ; typ = TInt
         ; rhs = TConst (CInt 2, TInt)
         ; body =
             TPrim
               { op = Add
               ; left = TName ("x", TInt)
               ; right = TName ("x", TInt)
               ; typ = TInt
               }
         })
      texp
      ~printer:show_typed_expr
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
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ fn_type ty;
    OUnit2.assert_equal
      (TFunDef
         ( "test"
         , [ "x", TInt ]
         , TLet
             { name = "y"
             ; typ = TBool
             ; rhs = TConst (CInt 2, TInt)
             ; body =
                 TPrim
                   { op = Eq
                   ; left = TName ("x", TInt)
                   ; right = TName ("y", TInt)
                   ; typ = TBool
                   }
             }
         , TBool ))
      texp
      ~printer:show_typed_expr
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
  | Some (ty, texp) ->
    OUnit2.assert_equal TInt ty;
    OUnit2.assert_equal (TConst (CInt 2, TInt)) texp ~printer:show_typed_expr
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
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ (TArrow (TInt, TInt)) ty;
    OUnit2.assert_equal
      (TFunDef
         ( "test"
         , [ "x", TInt ]
         , TPrim
             { op = Add
             ; left = TName ("x", TInt)
             ; right = TConst (CInt 2, TInt)
             ; typ = TInt
             }
         , TInt ))
      texp
      ~printer:show_typed_expr
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
