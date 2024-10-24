open Annotate

let rec free_vars = function
  | AConst _ -> []
  | AVar (x, _) -> [ x ]
  | ALam (args, body, _) ->
    let bound_vars = List.map fst args in
    List.filter (fun v -> not (List.mem v bound_vars)) (free_vars body)
  | AFunDef (_, args, body, _) ->
    let bound_vars = List.map fst args in
    List.filter (fun v -> not (List.mem v bound_vars)) (free_vars body)
  | AApp (f, args, _) ->
    List.fold_left (fun acc e -> acc @ free_vars e) (free_vars f) args
  | APrim (_, e1, e2, _) -> free_vars e1 @ free_vars e2
  | ALet (x, _, e1, e2) -> free_vars e1 @ List.filter (fun v -> v <> x) (free_vars e2)
;;

let make_fun_def name args body typ = AFunDef (name, args, body, typ)

let fresh_var =
  let counter = ref 0 in
  fun prefix ->
    incr counter;
    Printf.sprintf "%s_%d" prefix !counter
;;

let lift_lambdas globals expr =
  let defs = ref [] in
  let add_def def = defs := def :: !defs in
  let rec lift = function
    | AConst _ as c -> c
    | AVar _ as v -> v
    | ALam (args, body, typ) as lam ->
      let free = List.filter (fun v -> not (List.mem v globals)) (free_vars lam) in
      let fun_name = fresh_var "lambda" in
      (* WIP types *)
      let new_args = List.map (fun v -> v, TInt) free @ args in
      let fun_def = make_fun_def fun_name new_args (lift body) typ in
      add_def fun_def;
      let app =
        AApp (AVar (fun_name, typ), List.map (fun v -> AVar (v, TInt)) free, typ)
      in
      app
    | AFunDef (name, args, body, typ) -> AFunDef (name, args, lift body, typ)
    | AApp (f, args, typ) -> AApp (lift f, List.map lift args, typ)
    | APrim (op, e1, e2, typ) -> APrim (op, lift e1, lift e2, typ)
    | ALet (x, typ, e1, e2) -> ALet (x, typ, lift e1, lift e2)
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
