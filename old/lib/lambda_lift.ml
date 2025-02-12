open Source
open Infer

let rec free_vars ?(except = []) expr =
  let filter args v = (not (List.mem v args)) && not (List.mem v except) in
  match expr with
  | TConst _ -> []
  | TName (x, t) -> [ x, t ]
  | TLam { args; body; typ = _ } -> List.filter (filter args) (free_vars ~except body)
  | TFunDef (_, args, body, _) -> List.filter (filter args) (free_vars ~except body)
  | TApp { fn; args; typ = _ } ->
    List.fold_left (fun acc e -> acc @ free_vars ~except e) (free_vars ~except fn) args
  | TPrim { op = _; left = e1; right = e2; typ = _ } ->
    free_vars ~except e1 @ free_vars ~except e2
  | TLet { name; typ; rhs; body } ->
    free_vars ~except rhs
    @ List.filter (fun v -> v <> (name, typ)) (free_vars ~except body)
  | TIfThenElse { condition = cond; then_branch = e1; else_branch = e2; typ = _ } ->
    free_vars ~except cond @ free_vars ~except e1 @ free_vars ~except e2
  | TTuple (texps, _) -> List.fold_left (fun acc t -> free_vars ~except t @ acc) [] texps
  | TAccess (texp, _, _) -> free_vars ~except texp
;;

let fresh_var =
  let counter = ref 0 in
  fun prefix ->
    incr counter;
    Printf.sprintf "%s_%d" prefix !counter
;;

let rec subst_name old_name new_name expr =
  let subst_name = subst_name old_name new_name in
  match expr with
  | TConst _ | TName _ -> expr
  | TLam { args; body; typ } -> TLam { args; body = subst_name body; typ }
  | TFunDef (name, args, body, typ) -> TFunDef (name, args, subst_name body, typ)
  | TApp { fn = TName (n, t); args; typ } when n = old_name ->
    TApp { fn = TName (new_name, t); args = List.map subst_name args; typ }
  | TApp { fn; args; typ } ->
    TApp { fn = subst_name fn; args = List.map subst_name args; typ }
  | TPrim { op; left; right; typ } ->
    TPrim { op; left = subst_name left; right = subst_name right; typ }
  | TLet { name; typ; rhs; body } ->
    TLet { name; typ; rhs = subst_name rhs; body = subst_name body }
  | TIfThenElse { condition; then_branch; else_branch; typ } ->
    TIfThenElse
      { condition = subst_name condition
      ; then_branch = subst_name then_branch
      ; else_branch = subst_name else_branch
      ; typ
      }
  | TTuple (texps, typ) -> TTuple (List.map subst_name texps, typ)
  | TAccess (texp, idx, typ) -> TAccess (subst_name texp, idx, typ)
;;

let rec subst_expr_name new_expr old_name expr =
  let subst_expr_name = subst_expr_name new_expr old_name in
  match expr with
  | TConst _ | TName _ -> expr
  | TLam { args; body; typ } -> TLam { args; body = subst_expr_name body; typ }
  | TFunDef (name, args, body, typ) -> TFunDef (name, args, subst_expr_name body, typ)
  | TApp { fn = TName (n, _); args; typ } when n = old_name ->
    TApp { fn = new_expr; args = List.map subst_expr_name args; typ }
  | TApp { fn; args; typ } ->
    TApp { fn = subst_expr_name fn; args = List.map subst_expr_name args; typ }
  | TPrim { op; left; right; typ } ->
    TPrim { op; left = subst_expr_name left; right = subst_expr_name right; typ }
  | TLet { name; typ; rhs; body } ->
    TLet { name; typ; rhs = subst_expr_name rhs; body = subst_expr_name body }
  | TIfThenElse { condition; then_branch; else_branch; typ } ->
    TIfThenElse
      { condition = subst_expr_name condition
      ; then_branch = subst_expr_name then_branch
      ; else_branch = subst_expr_name else_branch
      ; typ
      }
  | TTuple (texps, typ) -> TTuple (List.map subst_expr_name texps, typ)
  | TAccess (texp, idx, typ) -> TAccess (subst_expr_name texp, idx, typ)
;;

let push_ts_tarrow typs tarrow = List.fold_right (fun x acc -> TFun (x, acc)) typs tarrow

let rec lift (defs : (sym * typ) list) expr =
  match expr with
  | TConst _ | TName _ -> expr, [], None
  | TLam { args; body; typ = fun_type } as lam ->
    let free = free_vars ~except:defs lam in
    let lifted_name = fresh_var "lambda" in
    let new_args = free @ args in
    let lifted_typ = push_ts_tarrow (List.map snd free) fun_type in
    let body', supercombinators1, _ = lift defs body in
    let fun_def = TFunDef (lifted_name, new_args, body', lifted_typ) in
    let app =
      if List.length free > 0
      then (
        let free_vars_exprs = List.map (fun (x, t) -> TName (x, t)) free in
        TApp
          { fn = TName (lifted_name, lifted_typ); args = free_vars_exprs; typ = fun_type })
      else TName (lifted_name, lifted_typ)
    in
    app, fun_def :: supercombinators1, Some lifted_name
  | TFunDef (name, args, body, typ) ->
    let body', supercombinators, _ = lift defs body in
    TFunDef (name, args, body', typ), supercombinators, None
  | TApp { fn; args; typ } ->
    let fn', supercombinators1, _ = lift defs fn in
    let args', arg_supercombinators =
      List.fold_left
        (fun (args, supercombinators) arg ->
           let arg', supercombinators', _ = lift defs arg in
           arg' :: args, supercombinators @ supercombinators')
        ([], [])
        args
    in
    let args' = List.rev args' in
    TApp { fn = fn'; args = args'; typ }, arg_supercombinators @ supercombinators1, None
  | TPrim { op; left; right; typ } ->
    let e1', supercombinators1, _ = lift defs left in
    let e2', supercombinators2, _ = lift defs right in
    ( TPrim { op; left = e1'; right = e2'; typ }
    , supercombinators1 @ supercombinators2
    , None )
  | TLet { name; typ; rhs = TLam _ as rhs; body } ->
    let lifted_expr, supercombinators1, lifted_name_opt = lift defs rhs in
    (match lifted_name_opt with
     | Some _new_name ->
       let body' = subst_expr_name lifted_expr name body in
       let body'', supercombinators2, _ = lift defs body' in
       body'', supercombinators1 @ supercombinators2, None
     | None ->
       let body', supercombinators2, _ = lift defs body in
       ( TLet { name; typ; rhs = lifted_expr; body = body' }
       , supercombinators1 @ supercombinators2
       , None ))
  | TLet { name; typ; rhs; body } ->
    let rhs', supercombinators1, _ = lift defs rhs in
    let body', supercombinators2, _ = lift defs body in
    ( TLet { name; typ; rhs = rhs'; body = body' }
    , supercombinators1 @ supercombinators2
    , None )
  | TIfThenElse { condition; then_branch; else_branch; typ } ->
    let condition', supercombinators1, _ = lift defs condition in
    let then_branch', supercombinators2, _ = lift defs then_branch in
    let else_branch', supercombinators3, _ = lift defs else_branch in
    ( TIfThenElse
        { condition = condition'
        ; then_branch = then_branch'
        ; else_branch = else_branch'
        ; typ
        }
    , supercombinators1 @ supercombinators2 @ supercombinators3
    , None )
  | TTuple (texps, typ) ->
    let texps', texp_supercombinators =
      List.fold_left
        (fun (texps, supercombinators) texp ->
           let texp', supercombinators', _ = lift defs texp in
           texp' :: texps, supercombinators @ supercombinators')
        ([], [])
        texps
    in
    TTuple (texps', typ), texp_supercombinators, None
  | TAccess (texp, idx, typ) ->
    let texp', texp_supercombinators, _ = lift defs texp in
    TAccess (texp', idx, typ), texp_supercombinators, None
;;

let lambda_lift defs expr =
  let lifted, globals, _ = lift defs expr in
  lifted, globals
;;
