open Infer

type global_def =
  { fundef : typed_expr
  ; name : sym
  ; ret_type : typ
  }
[@@deriving show]

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

let rec final_type ty =
  match ty with
  | TInt | TBool | TUnit | TVar _ -> ty
  | TArrow (_, t2) -> final_type t2
  | TList t -> final_type t
;;

module EliminatePartialApp = struct
  let var_name_counter = ref 0

  let unique_var_name x =
    incr var_name_counter;
    Printf.sprintf "#%s_%d" x !var_name_counter
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
    | TList t -> unpack_type t
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

module ForwardDeclataion = struct
  module FunTable = Map.Make (struct
      type t = int

      let compare = compare
    end)

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

  (*
     Hvis noget er en funktion, så skal vi lagre i tabellerne.
     Hvis noget er en app (app foregår altid med funktionsnavn), så skal vi ikke gøre noget vel?
     det er jo først på compile-tid at vi vil lave erstatningerne? eller er det egentlig ligegyldigt?
  *)
  let expr_to_tables expr name_idx idx_args_signature counter =
    match expr with
    | TFunDef (name, args, _, typ) ->
      ( Environment.add name counter name_idx
      , FunTable.add counter (args, typ) idx_args_signature
      , counter + 1 )
    | TConst _ | TName _ | TLam _ | TApp _ | TPrim _ | TLet _ | TIfThenElse _ ->
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
end

module EliminateConsecApp = struct
  let rec eliminate_consec expr =
    match expr with
    | TConst _ -> expr
    | TName _ -> expr
    | TPrim (op, e1, e2, t) -> TPrim (op, eliminate_consec e1, eliminate_consec e2, t)
    | TLet (x, t, e1, e2) -> TLet (x, t, eliminate_consec e1, eliminate_consec e2)
    (* An application where the "body" is itself an application *)
    | TApp (TApp (f', args', ty'), args, _) ->
      let combined_args = args' @ args in
      let resulting_expr = TApp (f', combined_args, final_type ty') in
      eliminate_consec resulting_expr
    | TApp (e1, e2, t) -> TApp (eliminate_consec e1, List.map eliminate_consec e2, t)
    | TFunDef (name, args, body, t) -> TFunDef (name, args, eliminate_consec body, t)
    | TLam (params, body, t) -> TLam (params, eliminate_consec body, t)
    | TIfThenElse (guard, guard_typ, then_branch, else_branch, typ) ->
      TIfThenElse
        ( eliminate_consec guard
        , guard_typ
        , eliminate_consec then_branch
        , eliminate_consec else_branch
        , typ )
  ;;
end

let optimize defs expr =
  let expr = ConstantFold.constant_fold_expr expr in
  let expr = EliminatePartialApp.eliminate_partial expr in
  let lifted, globals = Lambda_lift.lambda_lift defs expr in
  let lifted = EliminateConsecApp.eliminate_consec lifted in
  let globals = List.map EliminateConsecApp.eliminate_consec globals in
  (* print_endline (show_typed_expr lifted);
     if List.length globals == 0
     then print_endline ">>no globals<<"
     else List.iter (fun x -> show_typed_expr x |> print_endline) globals;*)
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
