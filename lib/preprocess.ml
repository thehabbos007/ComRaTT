open Source
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
    | TPrim
        { op = Add
        ; left = TConst (CInt n1, TInt)
        ; right = TConst (CInt n2, TInt)
        ; typ = TInt
        } -> TConst (CInt (n1 + n2), TInt)
    | TPrim
        { op = Sub
        ; left = TConst (CInt n1, TInt)
        ; right = TConst (CInt n2, TInt)
        ; typ = TInt
        } -> TConst (CInt (n1 - n2), TInt)
    | TPrim
        { op = Mul
        ; left = TConst (CInt n1, TInt)
        ; right = TConst (CInt n2, TInt)
        ; typ = TInt
        } -> TConst (CInt (n1 * n2), TInt)
    | TPrim { op; left; right; typ } ->
      TPrim { op; left = constant_fold_expr left; right = constant_fold_expr right; typ }
    | TLet { name; typ; rhs; body } ->
      TLet { name; typ; rhs = constant_fold_expr rhs; body = constant_fold_expr body }
    | TApp { fn; args; typ } ->
      TApp { fn = constant_fold_expr fn; args = List.map constant_fold_expr args; typ }
    | TFunDef (name, args, body, typ) -> TFunDef (name, args, constant_fold_expr body, typ)
    | TLam { args; body; typ } -> TLam { args; body = constant_fold_expr body; typ }
    | TIfThenElse { condition; then_branch; else_branch; typ } ->
      TIfThenElse
        { condition = constant_fold_expr condition
        ; then_branch = constant_fold_expr then_branch
        ; else_branch = constant_fold_expr else_branch
        ; typ
        }
    | TTuple (texps, typ) -> TTuple (List.map constant_fold_expr texps, typ)
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
  | TInt | TBool | TUnit -> ty
  | TFun (_, t2) -> final_type t2
  | TProduct ts -> final_type_tproduct ts

and final_type_tproduct = function
  | [] -> failwith "final_type_tproduct: Attempted to take final type of empty tproduct"
  | [ t ] -> t
  | _ :: ts -> final_type_tproduct ts
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
    | TPrim { op; left; right; typ = ty } ->
      TPrim { op; left = subst left; right = subst right; typ = ty }
    | TLam { args; body; typ = ty } -> TLam { args; body = subst body; typ = ty }
    | TApp { fn = func; args; typ = ty } ->
      TApp { fn = subst func; args = List.map subst args; typ = ty }
    | TLet { name; typ; rhs; body } when name <> bind_old ->
      TLet { name; typ; rhs = subst rhs; body = subst body }
    | TIfThenElse { condition; then_branch; else_branch; typ } ->
      TIfThenElse
        { condition = subst condition
        ; then_branch = subst then_branch
        ; else_branch = subst else_branch
        ; typ
        }
    | TConst _ | TLet _ | TName _ | TFunDef _ -> aexpr
    | TTuple (texps, typ) -> TTuple (List.map subst texps, typ)
  ;;

  let rec unpack_type ty =
    match ty with
    | TInt | TBool | TUnit -> []
    | TFun (t1, t2) -> t1 :: unpack_type t2
    | TProduct ts -> ts
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
    | TName (_name, TFun (_t1, _t2)) -> aexpr
    | TName _ -> aexpr
    | TPrim { op; left; right; typ } ->
      TPrim { op; left = eliminate_partial left; right = eliminate_partial right; typ }
    | TFunDef (name, args, body, ty) -> TFunDef (name, args, eliminate_partial body, ty)
    | TLam { args; body; typ } -> TLam { args; body = eliminate_partial body; typ }
    | TApp { fn; args; typ } ->
      let transformed_fn = eliminate_partial fn in
      let transformed_args = List.map eliminate_partial args in
      TApp { fn = transformed_fn; args = transformed_args; typ }
    | TLet { name = bind_old; rhs = TName (bind_new, _); body; _ } ->
      substitute_binding bind_old bind_new body
    (* A let binding where the right hand side is a partial application *)
    | TLet
        { name
        ; typ
        ; rhs = TApp { fn = lam; args; typ = TFun (_t1, _t2) as app_ty }
        ; body
        } ->
      let eta_args = unpack_type app_ty in
      let lambda_args, app_args = generate_lambda_vars_and_app_vars_no_tail eta_args in
      let new_lam =
        TLam
          { args = lambda_args
          ; body =
              TApp { fn = lam; args = List.append args app_args; typ = final_type app_ty }
          ; typ
          }
      in
      TLet { name; typ; rhs = new_lam; body = eliminate_partial body }
    | TLet { name; typ; rhs; body } ->
      TLet { name; typ; rhs = eliminate_partial rhs; body = eliminate_partial body }
    | TIfThenElse { condition; then_branch; else_branch; typ } ->
      TIfThenElse
        { condition = eliminate_partial condition
        ; then_branch = eliminate_partial then_branch
        ; else_branch = eliminate_partial else_branch
        ; typ
        }
    | TTuple (texps, typ) -> TTuple (List.map eliminate_partial texps, typ)
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
    | TConst _ | TName _ | TLam _ | TApp _ | TPrim _ | TLet _ | TIfThenElse _ | TTuple _
      -> name_idx, idx_args_signature, counter
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
    | TPrim { op; left; right; typ } ->
      TPrim { op; left = eliminate_consec left; right = eliminate_consec right; typ }
    | TLet { name; typ; rhs; body } ->
      TLet { name; typ; rhs = eliminate_consec rhs; body = eliminate_consec body }
    (* An application where the "body" is itself an application *)
    | TApp { fn = TApp { fn = f'; args = args'; typ = ty' }; args; typ = _ } ->
      let combined_args = args' @ args in
      let resulting_expr = TApp { fn = f'; args = combined_args; typ = final_type ty' } in
      eliminate_consec resulting_expr
    | TApp { fn; args; typ } ->
      TApp { fn = eliminate_consec fn; args = List.map eliminate_consec args; typ }
    | TFunDef (name, args, body, t) -> TFunDef (name, args, eliminate_consec body, t)
    | TLam { args; body; typ } -> TLam { args; body = eliminate_consec body; typ }
    | TIfThenElse { condition; then_branch; else_branch; typ } ->
      TIfThenElse
        { condition = eliminate_consec condition
        ; then_branch = eliminate_consec then_branch
        ; else_branch = eliminate_consec else_branch
        ; typ
        }
    | TTuple (texps, typ) -> TTuple (List.map eliminate_consec texps, typ)
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
