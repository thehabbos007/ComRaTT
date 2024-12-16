open Infer

let rec free_vars ?(except = []) expr =
  let filter args v = (not (List.mem v args)) && not (List.mem v except) in
  match expr with
  | TConst _ -> []
  | TName (x, t) -> [ x, t ]
  | TLam (args, body, _) -> List.filter (filter args) (free_vars ~except body)
  | TFunDef (_, args, body, _) -> List.filter (filter args) (free_vars ~except body)
  | TApp (f, args, _) ->
    List.fold_left (fun acc e -> acc @ free_vars ~except e) (free_vars ~except f) args
  | TPrim (_, e1, e2, _) -> free_vars ~except e1 @ free_vars ~except e2
  | TLet (x, t, e1, e2) ->
    free_vars ~except e1 @ List.filter (fun v -> v <> (x, t)) (free_vars ~except e2)
  | TIfThenElse (cond, _, e1, e2, _) ->
    free_vars ~except cond @ free_vars ~except e1 @ free_vars ~except e2
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
  | TLam (args, body, t) -> TLam (args, subst_name body, t)
  | TFunDef (name, args, body, typ) -> TFunDef (name, args, subst_name body, typ)
  | TApp (TName (n, t), args, typ) when n = old_name ->
    TApp (TName (new_name, t), List.map subst_name args, typ)
  | TApp (f, args, typ) -> TApp (subst_name f, List.map subst_name args, typ)
  | TPrim (op, e1, e2, typ) -> TPrim (op, subst_name e1, subst_name e2, typ)
  | TLet (x, t, e1, e2) -> TLet (x, t, subst_name e1, subst_name e2)
  | TIfThenElse (cond, t1, e1, e2, t2) ->
    TIfThenElse (subst_name cond, t1, subst_name e1, subst_name e2, t2)
;;

let rec subst_expr_name new_expr old_name expr =
  let subst_expr_name = subst_expr_name new_expr old_name in
  match expr with
  | TConst _ | TName _ -> expr
  | TLam (args, body, t) -> TLam (args, subst_expr_name body, t)
  | TFunDef (name, args, body, typ) -> TFunDef (name, args, subst_expr_name body, typ)
  | TApp (TName (n, _), args, typ) when n = old_name ->
    TApp (new_expr, List.map subst_expr_name args, typ)
  | TApp (f, args, typ) -> TApp (subst_expr_name f, List.map subst_expr_name args, typ)
  | TPrim (op, e1, e2, typ) -> TPrim (op, subst_expr_name e1, subst_expr_name e2, typ)
  | TLet (x, t, e1, e2) -> TLet (x, t, subst_expr_name e1, subst_expr_name e2)
  | TIfThenElse (cond, t1, e1, e2, t2) ->
    TIfThenElse (subst_expr_name cond, t1, subst_expr_name e1, subst_expr_name e2, t2)
;;

let push_ts_tarrow typs tarrow =
  List.fold_right (fun x acc -> TArrow (x, acc)) typs tarrow
;;

let rec lift (defs : (sym * typ) list) expr =
  match expr with
  | TConst _ | TName _ -> expr, [], None
  | TLam (args, body, fun_type) as lam ->
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
        TApp (TName (lifted_name, lifted_typ), free_vars_exprs, fun_type))
      else TName (lifted_name, lifted_typ)
    in
    app, fun_def :: supercombinators1, Some lifted_name
  | TFunDef (name, args, body, typ) ->
    let body', supercombinators, _ = lift defs body in
    TFunDef (name, args, body', typ), supercombinators, None
  | TApp (f, args, typ) ->
    let f', supercombinators1, _ = lift defs f in
    let args', arg_supercombinators =
      List.fold_left
        (fun (args, supercombinators) arg ->
          let arg', supercombinators', _ = lift defs arg in
          arg' :: args, supercombinators @ supercombinators')
        ([], [])
        args
    in
    let args' = List.rev args' in
    TApp (f', args', typ), arg_supercombinators @ supercombinators1, None
  | TPrim (op, e1, e2, typ) ->
    let e1', supercombinators1, _ = lift defs e1 in
    let e2', supercombinators2, _ = lift defs e2 in
    TPrim (op, e1', e2', typ), supercombinators1 @ supercombinators2, None
  | TLet (x, t, (TLam _ as e1), e2) ->
    let lifted_expr, supercombinators1, lifted_name_opt = lift defs e1 in
    (match lifted_name_opt with
     | Some _new_name ->
       let e2' = subst_expr_name lifted_expr x e2 in
       let e2'', supercombinators2, _ = lift defs e2' in
       e2'', supercombinators1 @ supercombinators2, None
     | None ->
       let e2', supercombinators2, _ = lift defs e2 in
       TLet (x, t, lifted_expr, e2'), supercombinators1 @ supercombinators2, None)
  | TLet (x, t, e1, e2) ->
    let e1', supercombinators1, _ = lift defs e1 in
    let e2', supercombinators2, _ = lift defs e2 in
    TLet (x, t, e1', e2'), supercombinators1 @ supercombinators2, None
  | TIfThenElse (cond, t1, e1, e2, t2) ->
    let cond', supercombinators1, _ = lift defs cond in
    let e1', supercombinators2, _ = lift defs e1 in
    let e2', supercombinators3, _ = lift defs e2 in
    ( TIfThenElse (cond', t1, e1', e2', t2)
    , supercombinators1 @ supercombinators2 @ supercombinators3
    , None )
;;

let lambda_lift defs expr =
  let lifted, globals, _ = lift defs expr in
  lifted, globals
;;
