open Infer

type global_def =
  { fundef : typed_expr
  ; name : sym
  ; ret_type : typ
  }
[@@deriving show]

(* Internal definitions, invisible to importers *)
open struct
  let name_counter = ref 0

  let unique_name x =
    incr name_counter;
    Printf.sprintf "#%s_%d" x !name_counter
  ;;

  let rec free_vars = function
    | TConst _ -> []
    | TName (x, t) -> [ x, t ]
    | TFunDef (name, params, body, _) ->
      let param_names = name :: List.map fst params in
      List.filter (fun (x, _) -> not (List.mem x param_names)) (free_vars body)
    | TLam (params, body, _) ->
      let param_names = List.map fst params in
      List.filter (fun (x, _) -> not (List.mem x param_names)) (free_vars body)
    | TApp (e1, e2, _) ->
      List.sort_uniq compare (free_vars e1 @ (List.map free_vars e2 |> List.concat))
    | TPrim (_, e1, e2, _) -> List.sort_uniq compare (free_vars e1 @ free_vars e2)
    | TLet (x, t, e1, e2) ->
      let fv1 = free_vars e1 in
      let fv2 = List.filter (( <> ) (x, t)) (free_vars e2) in
      List.sort_uniq compare (fv1 @ fv2)
    | _ -> failwith "ifthenelse"
  ;;

  let rec closure_convert expr =
    match expr with
    | TFunDef (name, params, body, t) -> TFunDef (name, params, closure_convert body, t)
    | TLam (params, body, t) ->
      let free_vars = free_vars expr in
      let new_params = free_vars @ params in
      let converted_body = closure_convert body in
      let lambda = TLam (new_params, converted_body, t) in
      let substitute_type = Infer.convert_params_to_arrow params t in
      TApp (lambda, List.map (fun (x, t) -> TName (x, t)) free_vars, substitute_type)
    | TApp (e1, e2, t) -> TApp (closure_convert e1, List.map closure_convert e2, t)
    | TPrim (op, e1, e2, t) -> TPrim (op, closure_convert e1, closure_convert e2, t)
    | TLet (x, t, e1, e2) -> TLet (x, t, closure_convert e1, closure_convert e2)
    | TConst _ | TName _ -> expr
    | _ -> failwith "ifthenelse"
  ;;

  let lift_lambdas expr =
    let definitions = ref [] in
    let rec lift expr =
      match expr with
      | TLam (params, body, ret_type) ->
        let lifted_body = lift body in
        let name = unique_name "global_lam" in
        definitions
        := { name; fundef = TFunDef (name, params, lifted_body, ret_type); ret_type }
           :: !definitions;
        TName (name, ret_type)
      | TFunDef (name, params, body, t) -> TFunDef (name, params, lift body, t)
      | TApp (e1, e2, t) -> TApp (lift e1, List.map lift e2, t)
      | TPrim (op, e1, e2, t) -> TPrim (op, lift e1, lift e2, t)
      | TLet (x, t, e1, e2) -> TLet (x, t, lift e1, lift e2)
      | TConst _ | TName _ -> expr
      | _ -> failwith "ifthenelse"
    in
    let lifted_expr = lift expr in
    lifted_expr, !definitions
  ;;
end

module Lift = struct
  let lambda_lift_expr expr =
    let closed_expr = closure_convert expr in
    lift_lambdas closed_expr
  ;;
end

module ConstantFold = struct
  (* Fold constants into expression that can be eliminated *)
  let rec constant_fold_expr expr =
    match expr with
    | TConst _ -> expr
    | TName _ -> expr
    | TPrim (Add, TConst (CInt n1, TInt), TConst (CInt n2, TInt), TInt) ->
      TConst (CInt (n1 + n2), TInt)
    | TPrim (Sub, TConst (CInt n1, TInt), TConst (CInt n2, TInt), TInt) ->
      TConst (CInt (n1 - n2), TInt)
    | TPrim (Mul, TConst (CInt n1, TInt), TConst (CInt n2, TInt), TInt) ->
      TConst (CInt (n1 * n2), TInt)
    | TPrim (op, e1, e2, t) -> TPrim (op, constant_fold_expr e1, constant_fold_expr e2, t)
    | TLet (x, t, e1, e2) -> TLet (x, t, constant_fold_expr e1, constant_fold_expr e2)
    | TApp (e1, e2, t) -> TApp (constant_fold_expr e1, List.map constant_fold_expr e2, t)
    | TFunDef (name, args, body, t) -> TFunDef (name, args, constant_fold_expr body, t)
    | TLam (params, body, t) -> TLam (params, constant_fold_expr body, t)
    | TIfThenElse (guard, guard_typ, then_branch, else_branch, typ) ->
      TIfThenElse
        ( constant_fold_expr guard
        , guard_typ
        , constant_fold_expr then_branch
        , constant_fold_expr else_branch
        , typ )
  ;;
end

let list_take n list =
  let rec aux n acc = function
    | [] -> None
    | _ when n = 0 -> Some (List.rev acc)
    | x :: xs -> aux (n - 1) (x :: acc) xs
  in
  aux n [] list
;;

module EliminatePartialApp = struct
  let var_name_counter = ref 0

  let unique_var_name x =
    incr name_counter;
    Printf.sprintf "#%s_%d" x !name_counter
  ;;

  let ( >> ) f g x = g (f x)

  let rec substitute_binding bind_old bind_new aexpr =
    let subst = substitute_binding bind_old bind_new in
    match aexpr with
    | TName (binding, ty) when binding = bind_old -> TName (bind_new, ty)
    | TPrim (op, e1, e2, ty) -> TPrim (op, subst e1, subst e2, ty)
    | TLam (args, body, ty) -> TLam (args, subst body, ty)
    | TApp (func, args, ty) -> TApp (subst func, List.map subst args, ty)
    | TLet (name, ty, rhs, body) when name <> bind_old ->
      TLet (name, ty, subst rhs, subst body)
    | TIfThenElse (guard, guard_typ, then_branch, else_branch, typ) ->
      TIfThenElse (subst guard, guard_typ, subst then_branch, subst else_branch, typ)
    | TConst _ | TLet _ | TName _ | TFunDef _ -> aexpr
  ;;

  let rec unpack_type ty =
    match ty with
    | TInt | TBool | TUnit -> []
    | TVar _ -> []
    | TArrow (t1, t2) -> t1 :: unpack_type t2
  ;;

  let rec final_type ty =
    match ty with
    | TInt | TBool | TUnit | TVar _ -> ty
    | TArrow (_, t2) -> final_type t2
  ;;

  let generate_names types =
    let rec aux types' acc =
      match types' with
      | [] -> acc
      | x :: xs -> aux xs ((unique_var_name "part_elim_lam", x) :: acc)
    in
    aux types []
  ;;

  (* A variant that is not tail recursive to avoid reversing lists *)
  let rec generate_lambda_vars_and_app_vars_no_tail eta_args =
    match eta_args with
    | [] -> [], []
    | typ :: types ->
      let name = unique_var_name "part_elim_lam" in
      let var = TName (name, typ) in
      let lambda, app = generate_lambda_vars_and_app_vars_no_tail types in
      (name, typ) :: lambda, var :: app
  ;;

  let rec eliminate_partial aexpr =
    match aexpr with
    | TConst _ -> aexpr
    | TName (_name, TArrow (_t1, _t2)) -> aexpr
    | TName _ -> aexpr
    | TPrim (op, e1, e2, ty) -> TPrim (op, eliminate_partial e1, eliminate_partial e2, ty)
    | TFunDef (name, args, body, ty) -> TFunDef (name, args, eliminate_partial body, ty)
    | TLam (args, body, ty) -> TLam (args, eliminate_partial body, ty)
    (* An application where the "body" is itself an application *)
    | TApp (TApp (f', args', ty'), args, _) ->
      let combined_args = args' @ args in
      let resulting_expr = TApp (f', combined_args, final_type ty') in
      eliminate_partial resulting_expr
    | TApp (func, args, ty) ->
      let transformed_func = eliminate_partial func in
      let transformed_args = List.map eliminate_partial args in
      TApp (transformed_func, transformed_args, ty)
    | TLet (bind_old, _ty, TName (bind_new, _), body) ->
      substitute_binding bind_old bind_new body
    (* A let binding where the right hand side is a partial application *)
    | TLet (name, ty, TApp (lam, args, (TArrow (_t1, _t2) as app_ty)), body) ->
      let eta_args = unpack_type app_ty in
      let lambda_args, app_args = generate_lambda_vars_and_app_vars_no_tail eta_args in
      let new_lam =
        TLam (lambda_args, TApp (lam, List.append args app_args, final_type app_ty), ty)
      in
      TLet (name, ty, new_lam, eliminate_partial body)
    | TLet (name, ty, rhs, body) ->
      TLet (name, ty, eliminate_partial rhs, eliminate_partial body)
    | TIfThenElse (guard, guard_typ, then_branch, else_branch, typ) ->
      TIfThenElse
        ( eliminate_partial guard
        , guard_typ
        , eliminate_partial then_branch
        , eliminate_partial else_branch
        , typ )
  ;;
end

let list_defs exprs =
  List.map
    (fun expr ->
      match expr with
      | TFunDef (name, _, _, t) -> name, t
      | _ -> failwith "Top level defs should be top level defs..")
    exprs
;;

(* let expr_to_funtable expr table =
   match expr with
   | AFunDef (name, args, body, typ) -> failwith ""
   | AConst _ | AVar _ | ALam _ | AApp _ | APrim _ | ALet _ | AIfThenElse _ -> table
   ;;
*)
(*
   Hvis noget er en funktion, så skal vi lagre dens navn sammen med index.
   Hvis noget er en app med et funktionsnavn, skal vi erstatte med noget der
   angiver funktionens index. Right?
*)
(* let expr_to_name_idx_table expr table counter =
   match expr with
   | AFunDef (name, args, body, typ) -> (, counter+1)
   | AConst _ | AVar _ | ALam _ | AApp _ | APrim _ | ALet _ | AIfThenElse _ -> (table, counter)

   let generate_function_table lifted globals =
   (* let lifted' = List.fold_left (fun acc expr -> expr_to_funtable expr acc) [] lifted in
   List.fold_left (fun acc expr -> expr_to_funtable expr acc) lifted' globals*)
   let global_name_idx = List.fold_left (fun (name_table, counter, final_table) expr -> ) (Int Environment, 0, []) globals in 42
   ;;*)

module FunTable = Map.Make (struct
    type t = int

    let compare = compare
  end)

(*
   Hvis noget er en funktion, så skal vi lagre i tabellerne.
   Hvis noget er en app (app foregår altid med funktionsnavn), så skal vi ikke gøre noget vel?
   det er jo først på compile-tid at vi vil lave erstatningerne? eller er det egentlig ligegyldigt?
*)
let expr_to_tables expr name_idx idx_args_signature counter =
  match expr with
  | AFunDef (name, args, _, typ) ->
    ( Environment.add name counter name_idx
    , FunTable.add counter (args, typ) idx_args_signature
    , counter + 1 )
  | AConst _ | AVar _ | ALam _ | AApp _ | APrim _ | ALet _ | AIfThenElse _ ->
    name_idx, idx_args_signature, counter
;;

let generate_function_tables lifted globals =
  let name_idx = Environment.empty in
  let idx_args_signature = FunTable.empty in
  let nidx, sigtable, _ =
    List.fold_left
      (fun (nid, res, counter) expr -> expr_to_tables expr nid res counter)
      (List.fold_left
         (fun (nid, res, counter) expr -> expr_to_tables expr nid res counter)
         (name_idx, idx_args_signature, 0)
         globals)
      lifted
  in
  nidx, sigtable
;;

let optimize defs expr =
  let expr = ConstantFold.constant_fold_expr expr in
  let eliminated = EliminatePartialApp.eliminate_partial expr in
  let lifted, globals = Lambda_lift.lambda_lift defs eliminated in
  print_endline (show_annot_expr lifted);
  if List.length globals == 0
  then print_endline ">>no globals<<"
  else List.iter (fun x -> show_annot_expr x |> print_endline) globals;
  lifted, globals
;;

let optimize_program exprs =
  let defs = list_defs exprs in
  List.fold_left
    (fun (fundefs, globals) expr ->
      let lifted, globals' = optimize defs expr in
      lifted :: fundefs, globals' @ globals)
    ([], [])
    exprs
;;
