open Annotate

let rec free_vars = function
  | AConst _ -> []
  | AVar (x, t) -> [ x, t ]
  | ALam (args, body, _) -> List.filter (fun v -> not (List.mem v args)) (free_vars body)
  | AFunDef (_, args, body, _) ->
    List.filter (fun v -> not (List.mem v args)) (free_vars body)
  | AApp (f, args, _) ->
    List.fold_left (fun acc e -> acc @ free_vars e) (free_vars f) args
  | APrim (_, e1, e2, _) -> free_vars e1 @ free_vars e2
  | ALet (x, t, e1, e2) ->
    free_vars e1 @ List.filter (fun v -> v <> (x, t)) (free_vars e2)
  | AIfThenElse (cond, _, e1, e2, _) -> free_vars cond @ free_vars e1 @ free_vars e2
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
  | AConst _ | AVar _ -> expr
  | ALam (args, body, t) -> ALam (args, subst_name body, t)
  | AFunDef (name, args, body, typ) -> AFunDef (name, args, subst_name body, typ)
  | AApp (AVar (n, t), args, typ) when n = old_name ->
    AApp (AVar (new_name, t), List.map subst_name args, typ)
  | AApp (f, args, typ) -> AApp (subst_name f, List.map subst_name args, typ)
  | APrim (op, e1, e2, typ) -> APrim (op, subst_name e1, subst_name e2, typ)
  | ALet (x, t, e1, e2) -> ALet (x, t, subst_name e1, subst_name e2)
  | AIfThenElse (cond, t1, e1, e2, t2) ->
    AIfThenElse (subst_name cond, t1, subst_name e1, subst_name e2, t2)
;;

let rec subst_expr_name new_expr old_name expr =
  let subst_expr_name = subst_expr_name new_expr old_name in
  match expr with
  | AConst _ | AVar _ -> expr
  | ALam (args, body, t) -> ALam (args, subst_expr_name body, t)
  | AFunDef (name, args, body, typ) -> AFunDef (name, args, subst_expr_name body, typ)
  | AApp (AVar (n, _), args, typ) when n = old_name ->
    AApp (new_expr, List.map subst_expr_name args, typ)
  | AApp (f, args, typ) -> AApp (subst_expr_name f, List.map subst_expr_name args, typ)
  | APrim (op, e1, e2, typ) -> APrim (op, subst_expr_name e1, subst_expr_name e2, typ)
  | ALet (x, t, e1, e2) -> ALet (x, t, subst_expr_name e1, subst_expr_name e2)
  | AIfThenElse (cond, t1, e1, e2, t2) ->
    AIfThenElse (subst_expr_name cond, t1, subst_expr_name e1, subst_expr_name e2, t2)
;;

let rec pop_n_tarrow n tarrow =
  match tarrow with
  | _ when n = 0 -> tarrow
  | TArrow (_, r) when n = 1 -> r
  | TArrow (_, r) -> pop_n_tarrow (n - 1) r
  | _ ->
    failwith ("Trying to pop " ^ string_of_int n ^ " args from " ^ string_of_type tarrow)
;;

let push_ts_tarrow typs tarrow =
  List.fold_right (fun x acc -> TArrow (x, acc)) typs tarrow
;;

(* Helper to count number of arguments expected by a type *)
let rec count_arrows = function
  | TArrow (_, ret) -> 1 + count_arrows ret
  | _ -> 0
;;

(* Helper to create a fully applied function call *)
let make_full_app f args ret_type = AApp (f, args, ret_type)

(* Create a wrapper function that ensures full application *)
let create_full_app_wrapper name args body typ =
  let expected_args = count_arrows typ in
  if List.length args = expected_args
  then AFunDef (name, args, body, typ)
  else (
    let missing = expected_args - List.length args in
    let new_args =
      List.init missing (fun i ->
        let arg_name = Printf.sprintf "arg_%d" i in
        arg_name, TInt
        (* Replace with proper type inference *))
    in
    let full_args = args @ new_args in
    let new_body =
      make_full_app
        (AVar (name, typ))
        (List.map (fun (x, t) -> AVar (x, t)) full_args)
        (pop_n_tarrow expected_args typ)
    in
    AFunDef (name ^ "_full", full_args, new_body, typ))
;;

let rec lift expr =
  match expr with
  | AConst _ | AVar _ -> expr, [], None
  | ALam (args, body, fun_type) as lam ->
    let free = free_vars lam in
    let lifted_name = fresh_var "lambda" in
    let new_args = free @ args in
    let lifted_typ = push_ts_tarrow (List.map snd free) fun_type in
    let body', supercombinators1, _ = lift body in
    let fun_def = AFunDef (lifted_name, new_args, body', lifted_typ) in
    let app =
      if List.length free > 0
      then (
        let free_vars_exprs = List.map (fun (x, t) -> AVar (x, t)) free in
        AApp (AVar (lifted_name, lifted_typ), free_vars_exprs, fun_type))
      else AVar (lifted_name, lifted_typ)
    in
    app, fun_def :: supercombinators1, Some lifted_name
  | AFunDef (name, args, body, typ) ->
    let body', supercombinators, _ = lift body in
    AFunDef (name, args, body', typ), supercombinators, None
  | AApp (f, args, typ) ->
    let f', supercombinators1, _ = lift f in
    let args', arg_supercombinators =
      List.fold_left
        (fun (args, supercombinators) arg ->
          let arg', supercombinators', _ = lift arg in
          arg' :: args, supercombinators @ supercombinators')
        ([], [])
        args
    in
    let args' = List.rev args' in
    AApp (f', args', typ), arg_supercombinators @ supercombinators1, None
  | APrim (op, e1, e2, typ) ->
    let e1', supercombinators1, _ = lift e1 in
    let e2', supercombinators2, _ = lift e2 in
    APrim (op, e1', e2', typ), supercombinators1 @ supercombinators2, None
  | ALet (x, t, (ALam _ as e1), e2) ->
    let lifted_expr, supercombinators1, lifted_name_opt = lift e1 in
    (match lifted_name_opt with
     | Some _new_name ->
       let e2' = subst_expr_name lifted_expr x e2 in
       let e2'', supercombinators2, _ = lift e2' in
       e2'', supercombinators1 @ supercombinators2, None
     | None ->
       let e2', supercombinators2, _ = lift e2 in
       ALet (x, t, lifted_expr, e2'), supercombinators1 @ supercombinators2, None)
  | ALet (x, t, e1, e2) ->
    let e1', supercombinators1, _ = lift e1 in
    let e2', supercombinators2, _ = lift e2 in
    ALet (x, t, e1', e2'), supercombinators1 @ supercombinators2, None
  | AIfThenElse (cond, t1, e1, e2, t2) ->
    let cond', supercombinators1, _ = lift cond in
    let e1', supercombinators2, _ = lift e1 in
    let e2', supercombinators3, _ = lift e2 in
    ( AIfThenElse (cond', t1, e1', e2', t2)
    , supercombinators1 @ supercombinators2 @ supercombinators3
    , None )
;;

let rec flatten_apps expr =
  match expr with
  | AConst _ | AVar _ -> expr
  | ALam (args, body, t) -> ALam (args, flatten_apps body, t)
  | AFunDef (name, args, body, typ) -> AFunDef (name, args, flatten_apps body, typ)
  | AApp (AApp (any, args, _), args', typ') ->
    AApp (flatten_apps any, List.map flatten_apps args @ List.map flatten_apps args', typ')
  | AApp (f, args, typ) -> AApp (flatten_apps f, List.map flatten_apps args, typ)
  | APrim (op, e1, e2, typ) -> APrim (op, flatten_apps e1, flatten_apps e2, typ)
  | ALet (x, t, e1, e2) -> ALet (x, t, flatten_apps e1, flatten_apps e2)
  | AIfThenElse (cond, t1, e1, e2, t2) ->
    AIfThenElse (flatten_apps cond, t1, flatten_apps e1, flatten_apps e2, t2)
;;

let lambda_lift expr =
  let lifted, globals, _ = lift expr in
  let lifted = flatten_apps lifted in
  let globals = List.map flatten_apps globals in
  lifted, globals
;;
