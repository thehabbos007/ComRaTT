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
  | TVar x -> string_of_int x
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

let rec push_args args =
  match args with
  | [] -> ""
  | head :: tail -> "i64.const " ^ acsti_to_str head ^ "\n" ^ push_args tail
;;

let create_outer ret_ty to_be_called name args =
  Printf.sprintf
    "%s (func $caller (result %s)\n\
    \ %s\n\
    \ call $%s \n\n\
     ) (export \"caller\" (func $caller))"
    to_be_called
    ret_ty
    (push_args args)
    name
;;

(*
   what is fastest: string concat or format strings?

   assume: lambdas are never nested and all lambdas are in a top level let binding
*)
let rec comp (expr : annot_expr) : string =
  match expr with
  | AVar (name, _) -> var_str name
  (* type of ACstI is always int, discard for now *)
  | ACstI (num, _) -> "i64.const " ^ string_of_int num
  | APrim (op, e1, e2, ty) ->
    let e1_comp = comp e1 in
    let e2_comp = comp e2 in
    e1_comp ^ "\n" ^ e2_comp ^ "\n" ^ binop_to_wasm op ty
  | AApp (name, args, _) ->
    apply_str (comp name) (List.fold_left (fun acc arg -> acc ^ comp arg) "" args)
  (* TODO rhs of let binding is not handled at all
     we still need to handle the call part of functions
     -> explicitly handled in the pattern below
  *)
  | ALet (_name, _ret_ty, ALam (_largs, _body, _t), AApp (_appname, _args, _ty)) ->
    create_outer
      (wasm_type_of_type _ret_ty)
      (lambda_let_str _name (wasm_type_of_type _ret_ty) (comp _body) _largs)
      _name
      _args
  | ALet (name, _, ALam (args, body, ret_ty), _) ->
    (* Maybe we put together the RHS of all let bindings
       in some glued together main function? *)
    lambda_let_str name (wasm_type_of_type ret_ty) (comp body) args
  | ALet (name, ret_ty, body, _) -> let_str name (wasm_type_of_type ret_ty) (comp body)
  | _ -> failwith "not supported"

and call_function name _args =
  Printf.sprintf
    "(call $%s (%s))"
    (comp name)
    (List.fold_left (fun acc arg -> acc ^ comp arg ^ " ") "" _args)

and comp_rhs rhs _lhs = Printf.sprintf "(%s)" (comp rhs)

let comp_global_defs (globals : Preprocess.global_def list) =
  List.fold_left
    (fun acc ({ name; body; ret_type; _ } : Preprocess.global_def) ->
      acc ^ comp (ALet (name, ret_type, body, ACstI (0, TInt))) ^ "\n")
    ""
    globals
;;

let init_wat (main_expr : annot_expr) (globals : Preprocess.global_def list) =
  Printf.sprintf "(module \n%s\n%s)" (comp_global_defs globals) (comp main_expr)
;;
