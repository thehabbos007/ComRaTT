open Annotate

type global_def =
  { name : sym
  ; args : (sym * typ) list
  ; body : annot_expr
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
    | ACstI _ -> []
    | AVar (x, t) -> [ x, t ]
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

  let closure_convert globals expr =
    let rec convert = function
      | ALet (x, t1, ALam (params, body, t2), e2) ->
        ALet (x, t1, ALam (params, convert body, t2), convert e2)
      | ALam (params, body, t) as e ->
        let free_vars =
          List.filter
            (fun (v, _) -> not (List.mem v globals || List.mem v (List.map fst params)))
            (free_vars e)
        in
        let new_params = free_vars @ params in
        let converted_body = convert body in
        let lambda = ALam (new_params, converted_body, t) in
        let substitute_type = convert_params_to_arrow params t in
        AApp (lambda, List.map (fun (x, t) -> AVar (x, t)) free_vars, substitute_type)
      | AApp (e1, e2, t) -> AApp (convert e1, List.map convert e2, t)
      | APrim (op, e1, e2, t) -> APrim (op, convert e1, convert e2, t)
      | ALet (x, t, e1, e2) -> ALet (x, t, convert e1, convert e2)
      | e -> e
    in
    convert expr
  ;;

  let lift_lambdas expr =
    let definitions = ref [] in
    let rec lift = function
      (* Important pattern: keep top-level lambda bindings,
         these are user-defined top-levelfunctions *)
      | ALet (x, t1, ALam (params, body, t2), e2) ->
        ALet (x, t1, ALam (params, lift body, t2), lift e2)
      | ALam (params, body, ret_type) ->
        let lifted_body = lift body in
        let name = unique_name "global_lam" in
        definitions
        := { name; args = params; body = lifted_body; ret_type } :: !definitions;
        AVar (name, ret_type)
      | AApp (e1, e2, t) -> AApp (lift e1, List.map lift e2, t)
      | APrim (op, e1, e2, t) -> APrim (op, lift e1, lift e2, t)
      | ALet (x, t, e1, e2) -> ALet (x, t, lift e1, lift e2)
      | e -> e
    in
    let lifted_expr = lift expr in
    lifted_expr, !definitions
  ;;
end

module Lift = struct
  let lambda_lift_expr globals expr =
    let closed_expr = closure_convert globals expr in
    lift_lambdas closed_expr
  ;;
end

module ConstantFold = struct
  (* Fold constants into expression that can be eliminated *)
  let rec constant_fold_expr expr =
    match expr with
    | ACstI _ -> expr
    | AVar _ -> expr
    | APrim (Add, ACstI (n1, TInt), ACstI (n2, TInt), TInt) -> ACstI (n1 + n2, TInt)
    | APrim (Sub, ACstI (n1, TInt), ACstI (n2, TInt), TInt) -> ACstI (n1 - n2, TInt)
    | APrim (Mul, ACstI (n1, TInt), ACstI (n2, TInt), TInt) -> ACstI (n1 * n2, TInt)
    | APrim (op, e1, e2, t) -> APrim (op, constant_fold_expr e1, constant_fold_expr e2, t)
    | ALet (x, t, e1, e2) -> ALet (x, t, constant_fold_expr e1, constant_fold_expr e2)
    | AApp (e1, e2, t) -> AApp (constant_fold_expr e1, List.map constant_fold_expr e2, t)
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
    | ACstI _ | ALet _ | AVar _ -> aexpr
  ;;

  let rec unpack_type ty =
    match ty with
    | TInt -> []
    | TVar _ -> []
    | TArrow (t1, t2) -> t1 :: unpack_type t2
  ;;

  let rec final_type ty =
    match ty with
    | TInt -> ty
    | TVar _ -> ty
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
    | ACstI _ -> aexpr
    | AVar (_name, TArrow (_t1, _t2)) -> aexpr
    | AVar _ -> aexpr
    | APrim (op, e1, e2, ty) -> APrim (op, eliminate_partial e1, eliminate_partial e2, ty)
    | ALam (args, body, ty) -> ALam (args, eliminate_partial body, ty)
    | AApp (AApp (f', args', ty'), args, _ty) ->
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

let optimize expr =
  let expr = ConstantFold.constant_fold_expr expr in
  Lift.lambda_lift_expr [] expr
;;
