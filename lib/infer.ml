open Source

type sym = string [@@deriving show, eq]

module Environment = Map.Make (struct
    type t = sym

    let compare = String.compare
  end)

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
  | TTuple of typed_expr list * typ
[@@deriving show, eq]

let rec build_fn_type args ret_ty =
  match args with
  | [] -> ret_ty
  | ty :: tys -> TFun (ty, build_fn_type tys ret_ty)
;;

let tfun_len_n ty n =
  let rec n_tfuns_to_list_rec ty n acc =
    match ty with
    | TFun (ty, next_ty) when n > 0 -> n_tfuns_to_list_rec next_ty (n - 1) (ty :: acc)
    | t when n = 0 -> t, List.rev acc
    | TInt | TBool | TUnit ->
      failwith @@ Printf.sprintf "Attempted to traverse a non-TFun type at n = %d" n
    | _ -> failwith "Too many arguments for function"
  in
  n_tfuns_to_list_rec ty n []
;;

let rec tproduct_from_type_list (inferred : typ list) =
  match inferred with
  | [] -> failwith "Failed to construct TProduct from list of tuple types and elements"
  | [ t; tlast ] -> TProduct (t, tlast)
  | t :: ts -> TProduct (t, tproduct_from_type_list ts)
;;

let get_with_custom_message opt message =
  match opt with
  | None -> failwith message
  | Some v -> v
;;

(* Refac to not use failwith. In general we want a Result type that can be bubbled up flatmap style *)
let rec infer ctx expr : (typ * typed_expr) option =
  match expr with
  | Tuple ts ->
    let inferred = ts |> List.map (fun t -> infer ctx t) in
    let types, tyexps =
      inferred
      |> List.map (fun t ->
        (*
           This deviates from the style of returning None given a failure, used everywhere else in infer/check
          But leave it until we refactor to Result
        *)
        get_with_custom_message t "Failed to infer type tuple due to element")
      |> List.split
    in
    let tproduct = tproduct_from_type_list types in
    Some (tproduct, TTuple (tyexps, tproduct))
  (*
     For Advance, look up the type of the name being advanced
    which should result in a thunk. Produce the return type of the thunk.
    Fail if not a thunk.
  *)
  | Advance name ->
    (match List.assoc_opt name ctx with
     | Some (TFun (TUnit, ty)) ->
       Some
         ( ty
         , TApp
             { fn = TName (name, TFun (TUnit, ty))
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
         ( TFun (TUnit, ty)
         , TLam { args = [ "#advance_unit", TUnit ]; body = texp; typ = TFun (TUnit, ty) }
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
    check the argument against, by popping the first type of the TFun.
    If the function is somehow not a TFun, that is a failure.
    If check returns some, then we we are good.
    *)
    (match infer ctx fn with
     | Some (TFun (ty, ret_ty), fn_texp) ->
       (match check ctx arg ty with
        | Some (_, arg_texp) ->
          Some (ret_ty, TApp { args = [ arg_texp ]; typ = ret_ty; fn = fn_texp })
        | None -> None)
     | Some (t, _) ->
       failwith ("Type of function was not TFun" ^ " " ^ show_typ t ^ " " ^ show_expr fn)
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
     A lambda checked against TFun.
  unify length of TFun with length of args
  add args to ctx
  check return type of TFun against body
  if that goes well, build the type of the lambda from the arguments and the return.
  TODO: support nested lambdas? (the len <> List.len args check. could we do like with fundef instead?)
  *)
  | Lam (args, body), TFun _ ->
    let ret_ty, types = tfun_len_n ty (List.length args) in
    let ctx_addition = List.combine args types in
    (match check (ctx_addition @ ctx) body ret_ty with
     | Some (_, texp) ->
       let lambda_type = build_fn_type types ret_ty in
       Some (lambda_type, TLam { args = ctx_addition; body = texp; typ = lambda_type })
     | None -> None)
  | Lam _, _ -> None (* Error: lambda type mismatch *)
  | expr, _ ->
    (match infer ctx expr with
     | Some (ty', texp) when ty = ty' -> Some (ty, texp)
     | _ -> None (* Error: type inference doesn't unify *))
;;

let infer_all exprs =
  let rec aux ctx acc = function
    | [] -> List.rev acc
    | fexpr :: rest ->
      (match fexpr with
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
       | FunDef (name, (TFun _ as ty), args, body) ->
         let ret_ty, types = tfun_len_n ty (List.length args) in
         let args_with_types = List.combine args types in
         let ctx_addition = (name, ty) :: args_with_types in
         let expanded_ctx = ctx_addition @ ctx in
         (match check expanded_ctx body ret_ty with
          | Some (body_ty, typed_body) ->
            let fn_ty = build_fn_type types body_ty in
            let typed_fun = TFunDef (name, args_with_types, typed_body, fn_ty) in
            aux expanded_ctx (typed_fun :: acc) rest
          | None -> failwith ("Error type checking function" ^ " " ^ name))
       (*
          A function with no arguments. Check the (constant) body against the type.
       Dont add name to context as we dont want a recursive constant function.
       *)
       | FunDef (name, typ, [], body) ->
         (match check ctx body typ with
          (*
             Function with no arguments.
            If check is successful, add the name and type to context, add typed function
            to accumulator and continue on tail.
          *)
          | Some (fun_ty, typed_body) ->
            let typed_fun = TFunDef (name, [], typed_body, fun_ty) in
            aux ((name, fun_ty) :: ctx) (typed_fun :: acc) rest
          | None ->
            failwith ("Error type checking function with no arguments" ^ " " ^ name))
       (*
          A non-annotated fundef is illegal.
       *)
       | FunDef _ -> failwith "Error: Non-annotated function")
  in
  let inferred = aux [] [] exprs in
  inferred
;;

let%test_unit "type checking a zero element tuple should fail" =
  let tuple = Tuple [] in
  OUnit2.assert_raises
    (Failure "Failed to construct TProduct from list of tuple types and elements")
    (fun () -> infer [] tuple)
;;

let%test_unit "type checking a single element tuple should fail" =
  let tuple = Tuple [ Const (CInt 42) ] in
  OUnit2.assert_raises
    (Failure "Failed to construct TProduct from list of tuple types and elements")
    (fun () -> infer [] tuple)
;;

let%test_unit "type checking the tuple ((42, true), false) should work" =
  let tuple =
    Tuple [ Tuple [ Const (CInt 42); Const (CBool true) ]; Const (CBool false) ]
  in
  let expected_first_elem_type = TProduct (TInt, TBool) in
  let expected_type = TProduct (expected_first_elem_type, TBool) in
  let expected =
    TTuple
      ( [ TTuple
            ( [ TConst (CInt 42, TInt); TConst (CBool true, TBool) ]
            , expected_first_elem_type )
        ; TConst (CBool false, TBool)
        ]
      , expected_type )
  in
  let inferred = infer [] tuple in
  match inferred with
  | Some (typ, texpr) ->
    OUnit2.assert_equal expected_type typ ~printer:show_typ;
    OUnit2.assert_equal texpr expected ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to infer type of tuple (42, true)"
;;

let%test_unit "type checking the tuple (42, true, false) should work" =
  let tuple = Tuple [ Const (CInt 42); Const (CBool true); Const (CBool false) ] in
  let inner_expected_type = TProduct (TBool, TBool) in
  let expected_type = TProduct (TInt, inner_expected_type) in
  let expected =
    TTuple
      ( [ TConst (CInt 42, TInt); TConst (CBool true, TBool); TConst (CBool false, TBool) ]
      , expected_type )
  in
  let inferred = infer [] tuple in
  match inferred with
  | Some (typ, texpr) ->
    OUnit2.assert_equal expected_type typ ~printer:show_typ;
    OUnit2.assert_equal texpr expected ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to infer type of tuple (42, true)"
;;

let%test_unit "type checking the tuple (42, true) should work" =
  let tuple = Tuple [ Const (CInt 42); Const (CBool true) ] in
  let expected_type = TProduct (TInt, TBool) in
  let expected =
    TTuple ([ TConst (CInt 42, TInt); TConst (CBool true, TBool) ], expected_type)
  in
  let inferred = infer [] tuple in
  match inferred with
  | Some (typ, texpr) ->
    OUnit2.assert_equal expected_type typ ~printer:show_typ;
    OUnit2.assert_equal texpr expected ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to infer type of tuple (42, true)"
;;

let%test_unit "infer_all on two functions where one calls the other should not fail" =
  let add_ty = TFun (TInt, TFun (TInt, TInt)) in
  let add_args = [ "x"; "y" ] in
  let add_body = Prim (Add, Var "x", Var "y") in
  let add_fn = FunDef ("add", add_ty, add_args, add_body) in
  let main_body = App (App (Var "add", Const (CInt 2)), Const (CInt 2)) in
  let main_ty = TInt in
  let main_fn = FunDef ("main", main_ty, [], main_body) in
  let inferred = infer_all [ add_fn; main_fn ] in
  OUnit2.assert_equal 2 (List.length inferred);
  match inferred with
  | [ TFunDef (add_name, add_args, add_body, add_ty)
    ; TFunDef (main_name, main_args, main_body, main_ty)
    ] ->
    OUnit2.assert_equal "add" add_name;
    OUnit2.assert_equal "main" main_name;
    OUnit2.assert_equal (TFun (TInt, TFun (TInt, TInt))) add_ty;
    OUnit2.assert_equal TInt main_ty;
    OUnit2.assert_equal [ "x", TInt; "y", TInt ] add_args;
    OUnit2.assert_equal [] main_args;
    OUnit2.assert_equal
      (TPrim { op = Add; left = TName ("x", TInt); right = TName ("y", TInt); typ = TInt })
      add_body
      ~printer:show_typed_expr;
    OUnit2.assert_equal
      (TApp
         { fn =
             TApp
               { fn = TName ("add", TFun (TInt, TFun (TInt, TInt)))
               ; args = [ TConst (CInt 2, TInt) ]
               ; typ = TFun (TInt, TInt)
               }
         ; typ = TInt
         ; args = [ TConst (CInt 2, TInt) ]
         })
      main_body
      ~printer:show_typed_expr
  | _ -> OUnit2.assert_failure "Expected two elements in the list"
;;

let%test_unit "infer_all on single arg function that adds arg to constant should not fail"
  =
  let fn_type = TFun (TInt, TInt) in
  let fn = FunDef ("test", fn_type, [ "x" ], Prim (Add, Var "x", Const (CInt 2))) in
  let inferred = infer_all [ fn ] in
  match List.nth inferred 0 with
  | TFunDef (name, args, body, ty) ->
    OUnit2.assert_equal "test" name ~printer:show_sym;
    OUnit2.assert_equal [ "x", TInt ] args;
    OUnit2.assert_equal
      (TPrim
         { op = Add; left = TName ("x", TInt); right = TConst (CInt 2, TInt); typ = TInt })
      body
      ~printer:show_typed_expr;
    OUnit2.assert_equal (TFun (TInt, TInt)) ty ~printer:show_typ
  | _ -> OUnit2.assert_failure "Not a fundef"
;;

let%test_unit "infer_all on single constant function should not fail" =
  let fn_type = TInt in
  let fn = FunDef ("test", fn_type, [], Const (CInt 2)) in
  let inferred = infer_all [ fn ] in
  OUnit2.assert_equal 1 (List.length inferred);
  match List.nth inferred 0 with
  | TFunDef (name, args, body, ty) ->
    OUnit2.assert_equal "test" name;
    OUnit2.assert_equal [] args;
    OUnit2.assert_equal (TConst (CInt 2, TInt)) body;
    OUnit2.assert_equal TInt ty
  | _ -> OUnit2.assert_failure "Not a fundef"
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

let%test_unit "build_lambda_type returns correct type" =
  let expected_type = TFun (TInt, TFun (TBool, TBool)) in
  let arg_types = [ TInt; TBool ] in
  let ret_ty = TBool in
  let actual_type = build_fn_type arg_types ret_ty in
  OUnit2.assert_equal expected_type actual_type
;;

let%test_unit "Valid function with shadowing should type check correctly" =
  let fn_type = TFun (TInt, TInt) in
  let fn_args = [ "x" ] in
  let fn_body = Let ("x", Const (CInt 40), Prim (Add, Var "x", Const (CInt 2))) in
  let fn = FunDef ("test", fn_type, fn_args, fn_body) in
  let inferred = infer_all [ fn ] in
  OUnit2.assert_equal 1 (List.length inferred);
  match List.nth inferred 0 with
  | TFunDef (name, args, body, ty) ->
    OUnit2.assert_equal "test" name;
    OUnit2.assert_equal [ "x", TInt ] args;
    OUnit2.assert_equal
      (TLet
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
         })
      body
      ~printer:show_typed_expr;
    OUnit2.assert_equal (TFun (TInt, TInt)) ty ~printer:show_typ
  | _ -> OUnit2.assert_failure "Failed to check valid function with shadowing"
;;

let%test_unit
    "Valid function with shadowing where types change should type check correctly"
  =
  let fn_type = TFun (TInt, TBool) in
  let fn_args = [ "x" ] in
  let fn_body = Let ("x", Const (CBool true), Var "x") in
  let fn = FunDef ("test", fn_type, fn_args, fn_body) in
  let inferred = infer_all [ fn ] in
  OUnit2.assert_equal 1 (List.length inferred);
  match List.nth inferred 0 with
  | TFunDef (name, args, body, ty) ->
    OUnit2.assert_equal "test" name;
    OUnit2.assert_equal [ "x", TInt ] args;
    OUnit2.assert_equal
      (TLet
         { name = "x"
         ; typ = TBool
         ; rhs = TConst (CBool true, TBool)
         ; body = TName ("x", TBool)
         })
      body
      ~printer:show_typed_expr;
    OUnit2.assert_equal (TFun (TInt, TBool)) ty ~printer:show_typ
  | _ ->
    OUnit2.assert_failure
      "Failed to check valid function with shadowing where types change"
;;

let%test_unit "Complex, valid function should type check correctly" =
  let fn_type = TFun (TInt, TInt) in
  let fn_args = [ "x" ] in
  let fn_body =
    IfThenElse
      ( Prim (Eq, Var "x", Const (CInt 2))
      , Let ("del", Delay (Prim (Mul, Var "x", Const (CInt 2))), Advance "del")
      , Prim (Add, Var "x", Const (CInt 40)) )
  in
  let fn = FunDef ("test", fn_type, fn_args, fn_body) in
  let inferred = infer_all [ fn ] in
  OUnit2.assert_equal 1 (List.length inferred);
  match List.nth inferred 0 with
  | TFunDef (name, args, body, ty) ->
    OUnit2.assert_equal "test" name;
    OUnit2.assert_equal [ "x", TInt ] args;
    OUnit2.assert_equal
      (TIfThenElse
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
                     ; typ = TFun (TUnit, TInt)
                     }
               ; body =
                   TApp
                     { fn = TName ("del", TFun (TUnit, TInt))
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
         })
      body
      ~printer:show_typed_expr;
    OUnit2.assert_equal (TFun (TInt, TInt)) ty ~printer:show_typ
  | _ -> OUnit2.assert_failure "Failed to check complex, valid function"
;;

let%test_unit "Checking a lambda against non-TFun type should fail" =
  let lam = Lam ([ "x" ], Const (CInt 2)) in
  match check [] lam TInt with
  | Some _ ->
    OUnit2.assert_failure "Should have failed to check type of lambda against non-TFun"
  | None ->
    OUnit2.assert_bool "Correctly failed to check type of lambda against non-TFun" true
;;

let%test_unit "Checking a valid lambda should not fail" =
  let lam = Lam ([ "x" ], Prim (Add, Var "x", Const (CInt 2))) in
  match check [] lam (TFun (TInt, TInt)) with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ (TFun (TInt, TInt)) ty;
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
         ; typ = TFun (TInt, TInt)
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to check valid lambda"
;;

let%test_unit "Invalid application should fail type checking" =
  let app = App (Var "f", Const (CBool true)) in
  match infer [ "f", TFun (TInt, TInt) ] app with
  | Some _ ->
    OUnit2.assert_failure "Should have failed to infer type of invalid application"
  | None ->
    OUnit2.assert_bool "Correctly failed to infer type of invalid application" true
;;

let%test_unit "Valid multiple application should type check correctly" =
  let app = App (App (Var "f", Const (CInt 2)), Const (CInt 2)) in
  let expected_inner_fn = TName ("f", TFun (TInt, TFun (TInt, TInt))) in
  let expected_inner_app =
    TApp
      { fn = expected_inner_fn
      ; typ = TFun (TInt, TInt)
      ; args = [ TConst (CInt 2, TInt) ]
      }
  in
  let expected_outer_app =
    TApp { fn = expected_inner_app; args = [ TConst (CInt 2, TInt) ]; typ = TInt }
  in
  match infer [ "f", TFun (TInt, TFun (TInt, TInt)) ] app with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal expected_outer_app texp ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to check valid application"
;;

let%test_unit "Valid application should type check correctly" =
  let app = App (Var "f", Const (CInt 2)) in
  match infer [ "f", TFun (TInt, TInt) ] app with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal
      (TApp
         { fn = TName ("f", TFun (TInt, TInt))
         ; args = [ TConst (CInt 2, TInt) ]
         ; typ = TInt
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to check valid application"
;;

let%test_unit "Infer Delay should produce a thunk" =
  let delayed = Delay (Const (CInt 2)) in
  match infer [] delayed with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ (TFun (TUnit, TInt)) ty;
    OUnit2.assert_equal
      (TLam
         { args = [ "#advance_unit", TUnit ]
         ; body = TConst (CInt 2, TInt)
         ; typ = TFun (TUnit, TInt)
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to infer type of delay"
;;

let%test_unit "Infer Advance should not fail when name is bound to a thunk" =
  let adv = Advance "x" in
  match infer [ "x", TFun (TUnit, TInt) ] adv with
  | Some (ty, texp) ->
    OUnit2.assert_equal ~printer:show_typ TInt ty;
    OUnit2.assert_equal
      (TApp
         { fn = TName ("x", TFun (TUnit, TInt))
         ; args = [ TConst (CUnit, TUnit) ]
         ; typ = TInt
         })
      texp
      ~printer:show_typed_expr
  | None -> OUnit2.assert_failure "Failed to infer type of advance"
;;

let%test_unit "Infer Advance should fail when name is not bound to a thunk" =
  let adv = Advance "x" in
  match infer [ "x", TFun (TInt, TInt) ] adv with
  | Some _ ->
    OUnit2.assert_failure
      "Should have failed to infer type of advance on name bound to non-thunk"
  | None ->
    OUnit2.assert_bool
      "Correctly failed to infer type of advance on name bound to non-thunk"
      true
;;

let%test_unit "Infer Advance should fail when name is not bound" =
  let adv = Advance "x" in
  match infer [] adv with
  | Some _ ->
    OUnit2.assert_failure "Should have failed to infer type of advance on name not bound"
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
  | None -> OUnit2.assert_failure "Failed to infer let binding"
;;

let%test_unit
    "Check conditional expression should fail when branches have different types"
  =
  let conditional =
    IfThenElse (Const (CBool true), Const (CInt 42), Const (CBool false))
  in
  match check [] conditional TInt with
  | Some _ ->
    OUnit2.assert_failure "Checking conditional with different branch types should fail"
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
  | None -> OUnit2.assert_failure "Failed to infer let binding"
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
  | None -> OUnit2.assert_failure "Failed to infer let binding"
;;

(*
   let%test_unit "Function with let-binding" =
  let fn_type = TFun (TInt, TBool) in
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
  let fn_type = TFun (TInt, TBool) in
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
  let fn_type = TFun (TInt, TInt) in
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
    OUnit2.assert_equal ~printer:show_typ (TFun (TInt, TInt)) ty;
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
*)

let%test_unit "TFun_len_n given non TFun type raises Failure" =
  OUnit2.assert_raises
    (Failure "Attempted to traverse a non-TFun type at n = 1")
    (fun () -> tfun_len_n TBool 1)
;;

let%test_unit
    "TFun_len_n given TFun type and n larger than amount of arguments raises Failure"
  =
  OUnit2.assert_raises
    (Failure "Attempted to traverse a non-TFun type at n = 1")
    (fun () -> tfun_len_n (TFun (TInt, TFun (TInt, TBool))) 3)
;;

let%test_unit "tfun_len_n given TFun and n=0 returns type unmodified" =
  let arrow = TFun (TInt, TBool) in
  let ret_ty, types = tfun_len_n arrow 0 in
  OUnit2.assert_equal arrow ret_ty;
  OUnit2.assert_equal [] types
;;

(* int -> bool *)
let%test_unit "TFun_len_n given TFun and n = args length returns correct result" =
  let arrow = TFun (TInt, TBool) in
  let ret_ty, types = tfun_len_n arrow 1 in
  OUnit2.assert_equal TBool ret_ty;
  OUnit2.assert_equal [ TInt ] types
;;

(* int -> int -> bool *)
let%test_unit "TFun_len_n given TFun and n = args length returns correct result" =
  let arrow = TFun (TInt, TFun (TInt, TBool)) in
  let ret_ty, types = tfun_len_n arrow 2 in
  OUnit2.assert_equal TBool ret_ty;
  OUnit2.assert_equal [ TInt; TInt ] types
;;

let rec print_list fn (v : 'a list) =
  match v with
  | [] -> ()
  | e :: l ->
    print_string @@ fn e;
    print_string " ";
    print_list fn l
;;

let%expect_test "TFun_len_n provides corect amount of arguments in the right order" =
  let typ, typs = tfun_len_n (TFun (TBool, TFun (TInt, TBool))) 2 in
  print_string @@ show_typ typ ^ ", ";
  print_list show_typ typs;
  [%expect {| Source.TBool, Source.TBool Source.TInt |}]
;;

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "()"
  | TFun (t1, t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TProduct (t1, t2) -> "(" ^ string_of_type t1 ^ ", " ^ string_of_type t2 ^ ")"
;;
