open Annotate

type global_def =
  { fundef : annot_expr
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
    | AConst _ -> []
    | AVar (x, t) -> [ x, t ]
    | AFunDef (name, params, body, _) ->
      let param_names = name :: List.map fst params in
      List.filter (fun (x, _) -> not (List.mem x param_names)) (free_vars body)
    | ALam (params, body, _) ->
      let param_names = List.map fst params in
      List.filter (fun (x, _) -> not (List.mem x param_names)) (free_vars body)
    | AApp (e1, e2, _) ->
      List.sort_uniq compare (free_vars e1 @ (List.map free_vars e2 |> List.concat))
    | APrim (_, e1, e2, _) -> List.sort_uniq compare (free_vars e1 @ free_vars e2)
    | ALet (x, t, e1, e2) ->
      let fv1 = free_vars e1 in
      let fv2 = List.filter (( <> ) (x, t)) (free_vars e2) in
      List.sort_uniq compare (fv1 @ fv2)
  ;;

  let convert_params_to_arrow params ret_type =
    List.fold_right (fun (_, t) acc -> TArrow (t, acc)) params ret_type
  ;;

  let rec closure_convert expr =
    match expr with
    | AFunDef (name, params, body, t) -> AFunDef (name, params, closure_convert body, t)
    | ALam (params, body, t) ->
      let free_vars = free_vars expr in
      let new_params = free_vars @ params in
      let converted_body = closure_convert body in
      let lambda = ALam (new_params, converted_body, t) in
      let substitute_type = convert_params_to_arrow params t in
      AApp (lambda, List.map (fun (x, t) -> AVar (x, t)) free_vars, substitute_type)
    | AApp (e1, e2, t) -> AApp (closure_convert e1, List.map closure_convert e2, t)
    | APrim (op, e1, e2, t) -> APrim (op, closure_convert e1, closure_convert e2, t)
    | ALet (x, t, e1, e2) -> ALet (x, t, closure_convert e1, closure_convert e2)
    | AConst _ | AVar _ -> expr
  ;;

  let lift_lambdas expr =
    let definitions = ref [] in
    let rec lift expr =
      match expr with
      | ALam (params, body, ret_type) ->
        let lifted_body = lift body in
        let name = unique_name "global_lam" in
        definitions
        := { name; fundef = AFunDef (name, params, lifted_body, ret_type); ret_type }
           :: !definitions;
        AVar (name, ret_type)
      | AFunDef (name, params, body, t) -> AFunDef (name, params, lift body, t)
      | AApp (e1, e2, t) -> AApp (lift e1, List.map lift e2, t)
      | APrim (op, e1, e2, t) -> APrim (op, lift e1, lift e2, t)
      | ALet (x, t, e1, e2) -> ALet (x, t, lift e1, lift e2)
      | AConst _ | AVar _ -> expr
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
    | AConst _ -> expr
    | AVar _ -> expr
    | APrim (Add, AConst (CInt n1, TInt), AConst (CInt n2, TInt), TInt) ->
      AConst (CInt (n1 + n2), TInt)
    | APrim (Sub, AConst (CInt n1, TInt), AConst (CInt n2, TInt), TInt) ->
      AConst (CInt (n1 - n2), TInt)
    | APrim (Mul, AConst (CInt n1, TInt), AConst (CInt n2, TInt), TInt) ->
      AConst (CInt (n1 * n2), TInt)
    | APrim (op, e1, e2, t) -> APrim (op, constant_fold_expr e1, constant_fold_expr e2, t)
    | ALet (x, t, e1, e2) -> ALet (x, t, constant_fold_expr e1, constant_fold_expr e2)
    | AApp (e1, e2, t) -> AApp (constant_fold_expr e1, List.map constant_fold_expr e2, t)
    | AFunDef (name, args, body, t) -> AFunDef (name, args, constant_fold_expr body, t)
    | ALam (params, body, t) -> ALam (params, constant_fold_expr body, t)
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
    | AVar (binding, ty) when binding = bind_old -> AVar (bind_new, ty)
    | APrim (op, e1, e2, ty) -> APrim (op, subst e1, subst e2, ty)
    | ALam (args, body, ty) -> ALam (args, subst body, ty)
    | AApp (func, args, ty) -> AApp (subst func, List.map subst args, ty)
    | ALet (name, ty, rhs, body) when name <> bind_old ->
      ALet (name, ty, subst rhs, subst body)
    | AConst _ | ALet _ | AVar _ | AFunDef _ -> aexpr
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
      let var = AVar (name, typ) in
      let lambda, app = generate_lambda_vars_and_app_vars_no_tail types in
      (name, typ) :: lambda, var :: app
  ;;

  let rec eliminate_partial aexpr =
    match aexpr with
    | AConst _ -> aexpr
    | AVar (_name, TArrow (_t1, _t2)) -> aexpr
    | AVar _ -> aexpr
    | APrim (op, e1, e2, ty) -> APrim (op, eliminate_partial e1, eliminate_partial e2, ty)
    | AFunDef (name, args, body, ty) -> AFunDef (name, args, eliminate_partial body, ty)
    | ALam (args, body, ty) -> ALam (args, eliminate_partial body, ty)
    (* An application where the "body" is itself an application *)
    | AApp (AApp (f', args', ty'), args, _) ->
      let combined_args = args' @ args in
      let resulting_expr = AApp (f', combined_args, final_type ty') in
      eliminate_partial resulting_expr
    | AApp (func, args, ty) ->
      let transformed_func = eliminate_partial func in
      let transformed_args = List.map eliminate_partial args in
      AApp (transformed_func, transformed_args, ty)
    | ALet (bind_old, _ty, AVar (bind_new, _), body) ->
      substitute_binding bind_old bind_new body
    (* A let binding where the right hand side is a partial application *)
    | ALet (name, ty, AApp (lam, args, (TArrow (_t1, _t2) as app_ty)), body) ->
      let eta_args = unpack_type app_ty in
      let lambda_args, app_args = generate_lambda_vars_and_app_vars_no_tail eta_args in
      let new_lam =
        ALam (lambda_args, AApp (lam, List.append args app_args, final_type app_ty), ty)
      in
      ALet (name, ty, new_lam, eliminate_partial body)
    | ALet (name, ty, rhs, body) ->
      ALet (name, ty, eliminate_partial rhs, eliminate_partial body)
  ;;
end

open Lambda_lift

let optimize expr =
  let expr = ConstantFold.constant_fold_expr expr in
  let eliminated = EliminatePartialApp.eliminate_partial expr in
  let lifted, globals = lift_lambdas [] eliminated in
  (* print_endline (show_annot_expr lifted);
     List.iter (fun x -> show_annot_expr x |> print_endline) globals;*)
  lifted, globals
;;
