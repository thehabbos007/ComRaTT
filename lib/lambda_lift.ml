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
;;

let make_fun_def name args body typ = AFunDef (name, args, body, typ)

let fresh_var =
  let counter = ref 0 in
  fun prefix ->
    incr counter;
    Printf.sprintf "%s_%d" prefix !counter
;;

let type_of expr =
  match expr with
  | AConst (_, t)
  | AVar (_, t)
  | ALam (_, _, t)
  | AFunDef (_, _, _, t)
  | AApp (_, _, t)
  | APrim (_, _, _, t)
  | ALet (_, t, _, _) -> t
;;

let shadow_assoc_put assoc x v = (x, v) :: assoc
let shadow_assoc_get x env = List.assoc_opt x env

let lift_lambdas globals expr =
  let defs = ref [] in
  let add_def def = defs := def :: !defs in
  let rec lift expr =
    match expr with
    | AConst _ | AVar _ -> expr
    | ALam (args, body, (TArrow (_arg_type, ret_type) as fun_type)) as lam ->
      let free = List.filter (fun v -> not (List.mem v globals)) (free_vars lam) in
      let fun_name = fresh_var "lambda" in
      let new_args = free @ args in
      let fun_def = make_fun_def fun_name new_args (lift body) fun_type in
      add_def fun_def;
      let app =
        AApp
          (AVar (fun_name, fun_type), List.map (fun (x, t) -> AVar (x, t)) free, ret_type)
      in
      app
    | ALam _ ->
      print_endline (show_annot_expr expr);
      failwith "Lambda with non-arrow type"
    | AFunDef (name, args, body, typ) -> AFunDef (name, args, lift body, typ)
    | AApp (f, args, typ) -> AApp (lift f, List.map lift args, typ)
    | APrim (op, e1, e2, typ) -> APrim (op, lift e1, lift e2, typ)
    | ALet (x, _, e1, e2) ->
      let e1' = lift e1 in
      ALet (x, type_of e1', e1', lift e2)
  in
  let lifted_expr = lift expr in
  lifted_expr, List.rev !defs
;;

let lift_all_lambdas globals exprs =
  let rec process acc_exprs acc_defs = function
    | [] -> List.rev acc_exprs, List.rev acc_defs
    | e :: rest ->
      let lifted, defs = lift_lambdas globals e in
      process (lifted :: acc_exprs) (defs @ acc_defs) rest
  in
  process [] [] exprs
;;
