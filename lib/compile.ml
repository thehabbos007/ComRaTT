open Source
open Annotate

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

(* den går ikke, for det er jo let der har navne...*)
let fun_string name args ret_typ =
  Printf.sprintf "(func $%s %s (result %s))" name (args_to_str args) ret_typ
;;

let var_str name = "local.get $" ^ name

(* what to do with let right hand side?*)
let let_str name ret_ty body =
  Printf.sprintf "(func $%s args? (result %s)\n    %s\n  )" name ret_ty body
;;

let lambda_let_str name ret_ty body args =
  Printf.sprintf
    "(func $%s %s (result %s)\n    %s\n  ) (export \"%s\" (func $%s))"
    name
    (args_to_str args)
    ret_ty
    body
    name
    name
;;

let apply_str name arg = Printf.sprintf "(call $%s (%s))" name arg

let acsti_to_str annot_expr =
  match annot_expr with
  | ACstI (number, _) -> string_of_int number
  | _ -> failwith "hanzo"
;;

(* duplicated from preprocess *)
let rec final_type ty =
  match ty with
  | TInt -> ty
  | TVar _ -> ty
  | TArrow (_, t2) -> final_type t2
;;

let rec lookup (names : (sym * typ * string) list) name =
  match names with
  | [] -> failwith "name not found"
  | (n, _, v) :: rest -> if n = name then v else lookup rest name
;;

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

let rec comp_new expr =
  match expr with
  | AVar (name, _) -> var_str name
  (* type of ACstI is always int, discard for now *)
  | ACstI (num, _) -> "i64.const " ^ string_of_int num
  | APrim (op, e1, e2, ty) ->
    let e1_comp = comp_new e1 in
    let e2_comp = comp_new e2 in
    e1_comp ^ "\n" ^ e2_comp ^ "\n" ^ binop_to_wasm op ty
  | AFunDef (name, args, body, ret_ty) ->
    let forward_dec =
      unfold_forward_decs (get_names_for_forward_declaration body Environment.empty)
    in
    Printf.sprintf
      "(func $%s %s (result %s)\n %s \n %s \n)"
      name
      (args_to_str args)
      (wasm_type_of_type (final_type ret_ty))
      forward_dec
      (comp_new body)
  | ALam _ -> failwith "lambda should have been lifted :("
  | ALet (name, _ty, rhs, AVar (name', _ty')) when name = name' ->
    let comp_rhs = comp_new rhs in
    Printf.sprintf "%s \n local.tee $%s" comp_rhs name
  | ALet (name, _ty, rhs, body) ->
    let comp_rhs = comp_new rhs in
    let set_name_to_rhs = Printf.sprintf "(local.set $%s (%s))" name comp_rhs in
    let comp_body = comp_new body in
    Printf.sprintf "%s\n %s" set_name_to_rhs comp_body
  | AApp (func, args, _ty) ->
    (* Assume that calling a function is done with a valid function name *)
    (match func with
     | AVar (name, _) ->
       Printf.sprintf
         "%s\ncall $%s"
         (List.fold_left (fun acc arg -> acc ^ comp_new arg ^ "\n") "" args)
         name
     | _ -> failwith "attempted calling a function that was not a valid AVar")
;;

let comp_global_defs (globals : Preprocess.global_def list) =
  List.fold_left
    (fun acc ({ name; fundef; ret_type; _ } : Preprocess.global_def) ->
      acc ^ comp_new (ALet (name, ret_type, fundef, ACstI (0, TInt))) ^ "\n")
    ""
    globals
;;

let rec comp_and_unfold_defs defs =
  match defs with
  | [] -> ""
  | def :: defs -> comp_new def ^ comp_and_unfold_defs defs
;;

let init_wasm (annot_exprs : annot_expr list) (globals : Preprocess.global_def list) =
  Printf.sprintf
    "(module \n %s\n %s\n (export \"main\" (func $main)))"
    (comp_global_defs globals)
    (comp_and_unfold_defs annot_exprs)
;;
