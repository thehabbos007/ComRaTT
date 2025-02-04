open Source
open Infer
open Preprocess

module WasmPrinter = struct
  let indent_level = ref 0
  let indent () = String.make (!indent_level * 2) ' '
  let newline () = "\n" ^ indent ()

  let with_indent f =
    incr indent_level;
    let result = f () in
    decr indent_level;
    result
  ;;
end

let gen_param ?(print_argument_name = true) name ty =
  if print_argument_name
  then Printf.sprintf "(param $%s %s)" name ty
  else Printf.sprintf "(param %s)" ty
;;

let gen_local name ty = Printf.sprintf "(local $%s %s)" name ty
let gen_result ty = Printf.sprintf "(result %s)" ty
let gen_const ty value = Printf.sprintf "(%s.const %s)" ty value
let gen_local_get name = Printf.sprintf "(local.get $%s)" name
let gen_local_set name = Printf.sprintf "(local.set $%s)" name
let gen_typeof_name name = Printf.sprintf "$type_of#%s" name

let binop_to_wasm op ty =
  match op, ty with
  | Add, TInt -> "(i64.add)"
  | Sub, TInt -> "(i64.sub)"
  | Mul, TInt -> "(i64.mul)"
  | Div, TInt -> "(i64.div_s)"
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
  | TFun (_t1, _t2) -> failwith "arrow type"
;;

let rec args_to_str ?(print_argument_name = true) (arg_list : (sym * typ) list) =
  match arg_list with
  | [] -> ""
  | (name, typ) :: tail ->
    gen_param ~print_argument_name name (wasm_type_of_type typ)
    ^ " "
    ^ args_to_str ~print_argument_name tail
;;

let generate_function_type ?(print_argument_name = true) args ret_ty =
  Printf.sprintf
    "%s%s"
    (args_to_str ~print_argument_name args)
    (gen_result (wasm_type_of_type (final_type ret_ty)))
;;

let var_str name = gen_local_get name

let rec generate_local_vars vars =
  match vars with
  | [] -> ""
  | (name, ty, _) :: vars ->
    gen_local name (wasm_type_of_type ty)
    ^ WasmPrinter.newline ()
    ^ generate_local_vars vars
;;

let rec get_names_for_forward_declaration expr map =
  match expr with
  | TLet { name; typ; body; _ } ->
    get_names_for_forward_declaration body (Environment.add name typ map)
  | TIfThenElse { condition; then_branch; else_branch; _ } ->
    get_names_for_forward_declaration
      condition
      (get_names_for_forward_declaration
         then_branch
         (get_names_for_forward_declaration else_branch map))
  | TPrim { left; right; _ } ->
    get_names_for_forward_declaration left (get_names_for_forward_declaration right map)
  | TLam _ -> failwith "no lambdas allowed"
  | TFunDef _ -> failwith "no fundefs allowed"
  | TConst _ | TName _ | TApp _ -> map
;;

let unfold_forward_decs decs =
  Environment.fold
    (fun name ty acc ->
       gen_local name (wasm_type_of_type ty) ^ WasmPrinter.newline () ^ acc)
    decs
    ""
;;

let gen_func_type name args typ =
  Printf.sprintf
    "(type %s (func %s))%s"
    (gen_typeof_name name)
    (generate_function_type ~print_argument_name:false args typ)
    (WasmPrinter.newline ())
;;

let rec comp expr name_sig_table =
  match expr with
  | TName (name, _) -> var_str name
  | TConst (CInt num, _) -> gen_const "i64" (string_of_int num)
  | TConst (CBool b, _) -> gen_const "i32" (if b then "1" else "0")
  | TConst (CUnit, _) -> gen_const "i32" "-1"
  | TPrim { op; left; right; typ } ->
    let e1_comp = WasmPrinter.with_indent (fun () -> comp left name_sig_table) in
    let e2_comp = WasmPrinter.with_indent (fun () -> comp right name_sig_table) in
    e1_comp
    ^ WasmPrinter.newline ()
    ^ e2_comp
    ^ WasmPrinter.newline ()
    ^ binop_to_wasm op typ
  | TFunDef (name, args, body, ret_ty) ->
    let forward_dec =
      unfold_forward_decs (get_names_for_forward_declaration body Environment.empty)
    in
    Printf.sprintf
      "%s%s(func $%s %s%s%s%s%s)%s"
      (WasmPrinter.newline ())
      (gen_func_type name args ret_ty)
      name
      (generate_function_type args ret_ty)
      (WasmPrinter.newline ())
      forward_dec
      (WasmPrinter.with_indent (fun () -> comp body name_sig_table))
      (WasmPrinter.newline ())
      (WasmPrinter.newline ())
  | TLam _ -> failwith "lambda should have been lifted :("
  | TLet { name; rhs; body = TName (name', _ty'); _ } when name = name' ->
    let comp_rhs = WasmPrinter.with_indent (fun () -> comp rhs name_sig_table) in
    Printf.sprintf "%s%s(local.tee $%s)" comp_rhs (WasmPrinter.newline ()) name
  | TLet { name; rhs; body; _ } ->
    let comp_rhs = WasmPrinter.with_indent (fun () -> comp rhs name_sig_table) in
    let set_name_to_rhs =
      Printf.sprintf "%s%s%s" comp_rhs (WasmPrinter.newline ()) (gen_local_set name)
    in
    let comp_body = WasmPrinter.with_indent (fun () -> comp body name_sig_table) in
    Printf.sprintf "%s%s%s" set_name_to_rhs (WasmPrinter.newline ()) comp_body
  | TApp { fn; args; _ } ->
    (* Assume that calling a function is done with a valid function name *)
    (match fn with
     | TName (name, _) ->
       Printf.sprintf
         "%s%s(call $%s)"
         (List.fold_left
            (fun acc arg ->
               acc
               ^ WasmPrinter.with_indent (fun () -> comp arg name_sig_table)
               ^ WasmPrinter.newline ())
            ""
            args)
         (WasmPrinter.newline ())
         name
     | _ -> failwith "attempted calling a function that was not a valid AVar")
  | TIfThenElse { condition; then_branch; else_branch; typ } ->
    (* The result part of the if should be left out if void, but we do not support that *)
    Printf.sprintf
      "(if %s%s%s%s(then%s%s)%s(else%s%s))"
      (gen_result (wasm_type_of_type typ))
      (WasmPrinter.newline ())
      (WasmPrinter.with_indent (fun () -> comp condition name_sig_table))
      (WasmPrinter.newline ())
      (WasmPrinter.newline ())
      (WasmPrinter.with_indent (fun () -> comp then_branch name_sig_table))
      (WasmPrinter.newline ())
      (WasmPrinter.newline ())
      (WasmPrinter.with_indent (fun () -> comp else_branch name_sig_table))
;;

let comp_global_defs (globals : Preprocess.global_def list) name_sig_table =
  List.fold_left
    (fun acc ({ name; fundef; ret_type; _ } : Preprocess.global_def) ->
       acc
       ^ comp
           (TLet { name; typ = ret_type; rhs = fundef; body = TConst (CInt 0, TInt) })
           name_sig_table
       ^ WasmPrinter.newline ())
    ""
    globals
;;

let rec comp_and_unfold_defs defs name_sig_table =
  match defs with
  | [] -> ""
  | def :: defs -> comp def name_sig_table ^ comp_and_unfold_defs defs name_sig_table
;;

let add_mem_region name memsize =
  Printf.sprintf "(memory $%s %s)%s" name (string_of_int memsize) (WasmPrinter.newline ())
;;

let rec args_to_sig_str (arg_list : (sym * typ) list) =
  match arg_list with
  | [] -> ")"
  | (_, typ) :: tail ->
    Printf.sprintf " %s %s" (wasm_type_of_type typ) (args_to_sig_str tail)
;;

let gen_func_table_entry count name =
  Printf.sprintf "(elem (i32.const %d) $%s)%s" count name (WasmPrinter.newline ())
;;

let forward_declare_func_table signatures =
  let table_def = Printf.sprintf "(table %d funcref)" (Environment.cardinal signatures) in
  (* We don't need this right now *)
  (*
  let gen_funcref_elem name =
    Printf.sprintf "(elem declare funcref (ref.func $%s))%s" name (WasmPrinter.newline ()) in
  let elem_declares =
    Environment.fold
      (fun name _ output -> gen_funcref_elem name ^ output)
      signatures
      (WasmPrinter.newline ())
  in *)
  let table_entries =
    Environment.fold
      (fun name (_, _, c) output -> gen_func_table_entry c name ^ output)
      signatures
      (WasmPrinter.newline ())
  in
  Printf.sprintf "%s%s%s" table_def (WasmPrinter.newline ()) table_entries
;;

(*
   TODO: the tables name_idx and funtable are currently inconvenient to use
   for e.g. "call_indirect" as that requires the wasm signature, which is not
   kept atm.
   Also: the values of arguments are not kept around (only name*type tuples), and they
   are also going to be needed for call_indirect. We might be able to extract
   that from the
*)
let init_wat
      (annot_exprs : typed_expr list)
      (globals : Preprocess.global_def list)
      name_sig_table
  =
  Printf.sprintf
    "(module%s%s%s%s%s%s%s%s%s(export \"main\" (func $main)))"
    (WasmPrinter.newline ())
    (add_mem_region "stable" 1)
    (add_mem_region "h1" 1)
    (add_mem_region "h2" 1)
    (WasmPrinter.newline ())
    (forward_declare_func_table name_sig_table)
    (comp_global_defs globals name_sig_table)
    (comp_and_unfold_defs annot_exprs name_sig_table)
    (WasmPrinter.newline ())
;;
