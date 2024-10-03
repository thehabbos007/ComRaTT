open Source
open Annotate

let binop_to_wasm op ty = match (op, ty) with
  | (Add, TInt) -> "i64.add"
  | (Sub, TInt) -> "i64.sub"
  | (Mul, TInt) -> "i64.mul" 
  | _ -> failwith "binop_to_wasm not supported for type"

let wasm_type_of_type ty = 
  match ty with
  | TInt -> "i64"
  | _ -> failwith "not implemented"

let rec args_to_str (arg_list : (sym * typ) list) =
  match arg_list with
  | [] -> ""
  | (name, typ)::tail -> Printf.sprintf "(param $%s %s) %s" name (wasm_type_of_type typ) (args_to_str tail)

(* den går ikke, for det er jo let der har navne...*)
let fun_string name args ret_typ =
  Printf.sprintf "(func $%s %s (result %s))" name (args_to_str args) ret_typ

let var_str name = "local.get $" ^ name

(* what to do with let right hand side?*)
let let_str name ret_ty body =
  Printf.sprintf "(func $%s args? (result %s)
    %s
  )" name ret_ty body

let lambda_let_str name ret_ty body args = 
  Printf.sprintf "(func $%s %s (result %s)
    %s
  ) (export \"%s\" (func $%s))" name (args_to_str args) ret_ty body name name

let apply_str name arg = Printf.sprintf "(call $%s (%s))" name arg



(*
  what is fastest: string concat or format strings? 
  
  assume: lambdas are never nested and all lambdas are in a top level let binding
*)
let rec comp (expr : annot_expr ) : string = 
  match expr with
  | AVar (name, _) -> var_str name
  (* type of ACstI is always int, discard for now *)
  | ACstI (num, _) -> "i64.const " ^  (string_of_int num)
  | APrim (op, e1, e2, ty) -> 
    let e1_comp = comp e1 in
    let e2_comp = comp e2 in
    e1_comp ^ "\n" ^ e2_comp ^ "\n" ^ binop_to_wasm op ty
  | AApp (name, args, _) -> apply_str (comp name) (List.fold_left (fun acc arg -> acc ^ (comp arg)) "" args)
  | ALet (name, ret_ty, ALam(args, body), _) -> lambda_let_str name (wasm_type_of_type ret_ty) (comp body) args
  | ALet (name, ret_ty, body, _) -> let_str name (wasm_type_of_type ret_ty) (comp body)
  | _ -> failwith "not supported"

let init_wat (expr : annot_expr) = 
  Printf.sprintf "(module \n%s)" (comp expr)