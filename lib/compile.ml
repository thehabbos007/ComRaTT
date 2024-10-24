open Source
open Annotate
open Preprocess

let binop_to_wasm op ty =
  match op, ty with
  | Add, TInt -> "i64.add"
  | Sub, TInt -> "i64.sub"
  | Mul, TInt -> "i64.mul"
  | _ -> failwith "binop_to_wasm not supported for type"
;;

let wasm_type_of_type ty =
  match ty with
  | TInt -> "i64"
  | TVar index -> failwith ("error: TVar found with index " ^ string_of_int index)
  | TArrow (_t1, _t2) -> "arrow"
;;

let rec args_to_str (arg_list : (sym * typ) list) =
  match arg_list with
  | [] -> ""
  | (name, typ) :: tail ->
    Printf.sprintf "(param $%s %s) %s" name (wasm_type_of_type typ) (args_to_str tail)
;;

let var_str name = "local.get $" ^ name

let rec generate_local_vars vars =
  match vars with
  | [] -> ""
  | (name, ty, _) :: vars ->
    Printf.sprintf "(local $%s %s)" name (wasm_type_of_type ty)
    ^ "\n"
    ^ generate_local_vars vars
;;

(* TODO: this is not entirely complete as the "_twofunctions" example that binds x, uses it and then rebinds it for new use, does not compile correctly *)
let rec get_names_for_forward_declaration expr map =
  match expr with
  | ALet (name, ty, _, body) ->
    get_names_for_forward_declaration body (Environment.add name ty map)
  | ALam _ -> failwith "no lambdas allowed"
  | AFunDef _ -> failwith "no fundefs allowed"
  | ACstI _ | AVar _ | APrim _ | AApp _ -> map
;;

let unfold_forward_decs decs =
  Environment.fold
    (fun name ty acc ->
      Printf.sprintf "(local $%s %s)\n" name (wasm_type_of_type ty) ^ acc)
    decs
    ""
;;

let rec comp expr =
  match expr with
  | AVar (name, _) -> var_str name
  (* type of ACstI is always int, discard for now *)
  | ACstI (num, _) -> "i64.const " ^ string_of_int num
  | APrim (op, e1, e2, ty) ->
    let e1_comp = comp e1 in
    let e2_comp = comp e2 in
    e1_comp ^ "\n" ^ e2_comp ^ "\n" ^ binop_to_wasm op ty
  | AFunDef (name, args, body, ret_ty) ->
    let forward_dec =
      unfold_forward_decs (get_names_for_forward_declaration body Environment.empty)
    in
    Printf.sprintf
      "(func $%s %s (result %s)\n %s \n %s \n)"
      name
      (args_to_str args)
      (wasm_type_of_type (EliminatePartialApp.final_type ret_ty))
      forward_dec
      (comp body)
  | ALam _ -> failwith "lambda should have been lifted :("
  | ALet (name, _ty, rhs, AVar (name', _ty')) when name = name' ->
    let comp_rhs = comp rhs in
    Printf.sprintf "%s \n local.tee $%s" comp_rhs name
  | ALet (name, _ty, rhs, body) ->
    let comp_rhs = comp rhs in
    let set_name_to_rhs = Printf.sprintf "(local.set $%s (%s))" name comp_rhs in
    let comp_body = comp body in
    Printf.sprintf "%s\n %s" set_name_to_rhs comp_body
  | AApp (func, args, _ty) ->
    (* Assume that calling a function is done with a valid function name *)
    (match func with
     | AVar (name, _) ->
       Printf.sprintf
         "%s\ncall $%s"
         (List.fold_left (fun acc arg -> acc ^ comp arg ^ "\n") "" args)
         name
     | _ -> failwith "attempted calling a function that was not a valid AVar")
;;

let comp_global_defs (globals : Preprocess.global_def list) =
  List.fold_left
    (fun acc ({ name; fundef; ret_type; _ } : Preprocess.global_def) ->
      acc ^ comp (ALet (name, ret_type, fundef, ACstI (0, TInt))) ^ "\n")
    ""
    globals
;;

let rec comp_and_unfold_defs defs =
  match defs with
  | [] -> ""
  | def :: defs -> comp def ^ comp_and_unfold_defs defs
;;

let init_wat (annot_exprs : annot_expr list) (globals : Preprocess.global_def list) =
  Printf.sprintf
    "(module \n %s\n %s\n (export \"main\" (func $main)))"
    (comp_global_defs globals)
    (comp_and_unfold_defs annot_exprs)
;;
