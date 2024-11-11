open Source
open Infer
open Preprocess

(* Comparison operators are signed for now *)
let binop_to_wasm op ty =
  match op, ty with
  | Add, TInt -> "(i64.add)"
  | Sub, TInt -> "(i64.sub)"
  | Mul, TInt -> "(i64.mul)"
  | Eq, TBool -> "(i64.eq)"
  | Lt, TBool -> "(i64.lt_s)"
  | Lte, TBool -> "(i64.le_s)"
  | Gt, TBool -> "(i64.gt_s)"
  | Gte, TBool -> "(i64.ge_s)"
  | Neq, TBool -> "(i64.ne)"
  | _ -> failwith ("binop_to_wasm not supported for type" ^ show_typ ty)
;;

let wasm_type_of_type ty =
  match ty with
  | TInt -> "i64"
  | TBool -> "i32"
  | TUnit -> "i32"
  | TVar tv -> failwith ("error: TVar found with index " ^ string_of_type_var_kind tv)
  | TArrow (_t1, _t2) -> "arrow"
;;

let rec args_to_str (arg_list : (sym * typ) list) =
  match arg_list with
  | [] -> ""
  | (name, typ) :: tail ->
    Printf.sprintf "(param $%s %s) %s" name (wasm_type_of_type typ) (args_to_str tail)
;;

let var_str name = "(local.get $" ^ name ^ ")"

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
  | TLet (name, ty, _, body) ->
    get_names_for_forward_declaration body (Environment.add name ty map)
  | TLam _ -> failwith "no lambdas allowed"
  | TFunDef _ -> failwith "no fundefs allowed"
  | TConst _ | TName _ | TPrim _ | TApp _ | TIfThenElse _ -> map
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
  | TName (name, _) -> var_str name
  (* type of ACstI is always int, discard for now *)
  | TConst (CInt num, _) -> "(i64.const " ^ string_of_int num ^ ")"
  | TConst (CBool b, _) -> "(i32.const " ^ if b then "1" else "0" ^ ")"
  | TConst (CUnit, _) -> "(i32.const " ^ "-1)"
  | TPrim (op, e1, e2, ty) ->
    let e1_comp = comp e1 in
    let e2_comp = comp e2 in
    e1_comp ^ "\n" ^ e2_comp ^ "\n" ^ binop_to_wasm op ty
  | TFunDef (name, args, body, ret_ty) ->
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
  | TLam _ -> failwith "lambda should have been lifted :("
  | TLet (name, _ty, rhs, TName (name', _ty')) when name = name' ->
    let comp_rhs = comp rhs in
    Printf.sprintf "%s \n local.tee $%s" comp_rhs name
  | TLet (name, _ty, rhs, body) ->
    let comp_rhs = comp rhs in
    let set_name_to_rhs = Printf.sprintf "%s (local.set $%s)" comp_rhs name in
    let comp_body = comp body in
    Printf.sprintf "%s\n %s" set_name_to_rhs comp_body
  | TApp (func, args, _ty) ->
    (* Assume that calling a function is done with a valid function name *)
    (match func with
     | TName (name, _) ->
       Printf.sprintf
         "%s\ncall $%s"
         (List.fold_left (fun acc arg -> acc ^ comp arg ^ "\n") "" args)
         name
     | _ -> failwith "attempted calling a function that was not a valid AVar")
  | TIfThenElse (guard, _guard_typ, then_branch, else_branch, branch_type) ->
    (* The result part of the if should be left out if void, but we do not support that *)
    Printf.sprintf
      "%s (if (result %s) (then %s) (else %s))"
      (comp guard)
      (wasm_type_of_type branch_type)
      (comp then_branch)
      (comp else_branch)
;;

let comp_global_defs (globals : Preprocess.global_def list) =
  List.fold_left
    (fun acc ({ name; fundef; ret_type; _ } : Preprocess.global_def) ->
      acc ^ comp (TLet (name, ret_type, fundef, TConst (CInt 0, TInt))) ^ "\n")
    ""
    globals
;;

let rec comp_and_unfold_defs defs =
  match defs with
  | [] -> ""
  | def :: defs -> comp def ^ comp_and_unfold_defs defs
;;

let add_function_table name size =
  Printf.sprintf "\n(table $%s %s funcref)" name (string_of_int size)
;;

let add_mem_region name memsize =
  Printf.sprintf "\n(memory $%s %s)" name (string_of_int memsize)
;;

let init_wat (annot_exprs : typed_expr list) (globals : Preprocess.global_def list) =
  Printf.sprintf
    "(module %s %s %s %s \n %s\n %s\n (export \"main\" (func $main)))"
    (add_mem_region "stable" 1)
    (add_mem_region "h1" 1)
    (add_mem_region "h2" 1)
    (* TODO
       - we need to do the size bookkeeping as well as shrinking/growing tables
       - max size?
    *)
    (add_function_table "funcs" 1)
    (comp_global_defs globals)
    (comp_and_unfold_defs annot_exprs)
;;
