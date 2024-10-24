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
    (fun acc ({ name; fundef; ret_type; _ } : Preprocess.global_def) ->
      acc ^ comp (ALet (name, ret_type, fundef, ACstI (0, TInt))) ^ "\n")
    ""
    globals
;;

let init_wat (main_expr : annot_expr) (globals : Preprocess.global_def list) =
  Printf.sprintf "(module \n%s\n%s)" (comp_global_defs globals) (comp main_expr)
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

let rec get_names_with_map expr map =
  match expr with
  | ALet (name, ty, _, body) -> get_names_with_map body (Environment.add name ty map)
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

let rec get_names_for_forward_declaration expr =
  match expr with
  (* A let binding is important.
     We need to collect the name and the type and then call
     recursively in both the rhs and the body.
     Nope, not the rhs. Let bindings in there are only
     temporary and used for giving a value to name.

     TODO: how is shadowing handled here? i guess the first value
     will be hit during a lookup, which is bad. Handle appropriately.
  *)
  | ALet (name, ty, _, body) ->
    Printf.sprintf "(local $%s %s)\n" name (wasm_type_of_type ty)
    ^ get_names_for_forward_declaration body
  | ALam _ -> failwith "no lambdas allowed"
  | AFunDef _ -> failwith "no fundefs allowed"
  | ACstI _ | AVar _ | APrim _ | AApp _ -> ""
;;

let rec comp_new expr =
  match expr with
  | AVar (name, _) -> var_str name
  (* type of ACstI is always int, discard for now *)
  | ACstI (num, _) -> "i64.const " ^ string_of_int num
  | APrim (op, e1, e2, ty) ->
    let e1_comp = comp e1 in
    let e2_comp = comp e2 in
    e1_comp ^ "\n" ^ e2_comp ^ "\n" ^ binop_to_wasm op ty
  | AFunDef (name, args, body, ret_ty) ->
    (* let forward_dec = get_names_for_forward_declaration body types in*)
    let forward_dec = unfold_forward_decs (get_names_with_map body Environment.empty) in
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
    (*
       let comp_rhs = comp_new rhs types in
       let set_name_to_rhs = Printf.sprintf "(local.set $%s (%s))" name comp_rhs in
       let names = [ name, ty, comp_rhs ] in
       let compiled_let = comp_let body names types in
       (* let _ = print_endline ("compiled body: " ^ compiled_let ^ "\ncompiled body stop") in*)
       (* let _ = print_endline ("compiled rhs: " ^ comp_rhs ^ "\ncompiled rhs stop") in*)
       Printf.sprintf
       "%s\n %s\n %s"
       (generate_local_vars names types)
       set_name_to_rhs
       compiled_let*)
    (*
       Printf.sprintf
       "(local %s) \n %s \n (local.set %s)"
       (wasm_type_of_type ty)
       "banan"
       "banan"*)
  | AApp (func, args, _ty) ->
    (* Assume that calling a function is done with a valid function name *)
    (match func with
     | AVar (name, _) ->
       Printf.sprintf
         "%s\ncall $%s"
         (List.fold_left (fun acc arg -> acc ^ comp_new arg ^ "\n") "" args)
         name
     | _ -> failwith "attempted calling a function")
(*
   Printf.sprintf
   "call $%s %s"
   (comp_new func types)
   (List.fold_left (fun acc arg -> acc ^ comp_new arg types ^ " ") "" args)
*)

(* Jeg har brug for at returnere en tuple her vel, så jeg kan bobble alle
   navne tilbage til toppen, så de kan defineres i starten af funktionen.
*)
and comp_let let_body names =
  match let_body with
  (* Hvis en let-bindings krop bare er en variabel, som i: let x = 42 in x
     skal vi skubbe 42 på stakken og så bruge local.tee $x
     Husk at x skal være defineret på forhånd, men det klares jo af den anden funktion.
  *)
  | AVar (name, _ty) ->
    let value = lookup names name in
    Printf.sprintf "%s  \n local.tee $%s" value name
  | others -> comp_new others
;;

let rec comp_and_unfold_defs defs =
  match defs with
  | [] -> ""
  | def :: defs -> comp_new def ^ comp_and_unfold_defs defs
;;

(* | def :: defs -> comp_and_unfold_defs defs types ^ comp_new def types*)

let wasm (annot_exprs : annot_expr list) (globals : Preprocess.global_def list) =
  Printf.sprintf
    "(module \n %s\n %s\n (export \"main\" (func $main)))"
    (comp_global_defs globals)
    (comp_and_unfold_defs annot_exprs)
;;
