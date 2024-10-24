open Source

(** Symbol i.e. variables *)
type sym = string [@@deriving show, eq]

module Environment = Map.Make (struct
    type t = sym

    let compare = String.compare
  end)

type typ =
  | TInt
  | TBool
  | TUnit
  | TVar of int
  | TArrow of typ * typ
[@@deriving show, eq]

(** Annotated expression (Types) *)
type annot_expr =
  | AConst of const * typ
  | AVar of sym * typ
  (* TODO: Consider top-level let bindings (top-level constants that are evaluated at the start of the program)
     One thing we have to consider is if we should handle functions that refer to top-level let bindings.
     When we introduce delay/adv, the top level bindings may need to be allocated in the heap at the start of the program.
  *)
  | ALam of (sym * typ) list * annot_expr * typ
  | AFunDef of sym * (sym * typ) list * annot_expr * typ
  | AApp of annot_expr * annot_expr list * typ
  | APrim of binop * annot_expr * annot_expr * typ
  | ALet of sym * typ * annot_expr * annot_expr
[@@deriving show, eq]

(* Type inference *)
let type_counter = ref 0

let fresh_type () =
  incr type_counter;
  TVar !type_counter
;;

type substitution = (int * typ) list

let rec apply_subst subst typ =
  match typ with
  | TBool | TInt | TUnit -> typ
  | TVar n ->
    (try List.assoc n subst with
     | Not_found -> TVar n)
  | TArrow (t1, t2) -> TArrow (apply_subst subst t1, apply_subst subst t2)
;;

let rec unify subst t1 t2 =
  let t1 = apply_subst subst t1 in
  let t2 = apply_subst subst t2 in
  match t1, t2 with
  | TVar n1, TVar n2 when n1 = n2 -> subst
  | TInt, TInt -> subst
  | TArrow (a1, r1), TArrow (a2, r2) ->
    let subst' = unify subst a1 a2 in
    unify subst' r1 r2
  | TVar n, t | t, TVar n ->
    if occurs n t then failwith "occurs check" else (n, t) :: subst
  | _t1, _t2 -> failwith "Type mismatch"

and occurs n = function
  | TVar m -> n = m
  | TArrow (a, r) -> occurs n a || occurs n r
  | TInt | TBool | TUnit -> false
;;

let rec construct_arrow_typ ret_typ = function
  | [] -> ret_typ
  | (_, ty) :: rest -> TArrow (ty, construct_arrow_typ ret_typ rest)
;;

(* follows https://stanford-cs242.github.io/f19/lectures/02-2-type-systems partly*)
let rec infer env subst = function
  | Const (CInt _) -> subst, TInt
  | Const (CBool _) -> subst, TBool
  | Const CUnit -> subst, TUnit
  | Delay e ->
    let subst', t = infer env subst e in
    subst', TArrow (TUnit, t)
  | Advance e ->
    let subst1, t = infer env subst e in
    let inner_type = fresh_type () in
    let subst2 = unify subst1 t (TArrow (TUnit, inner_type)) in
    subst2, inner_type
  | Var x ->
    (try
       let t = List.assoc x env in
       subst, apply_subst subst t
     with
     | Not_found -> failwith ("unbound variable: " ^ x))
  | FunDef (_name, args, e) ->
    let arg_types = List.map (fun arg -> arg, fresh_type ()) args in
    let subst', result_type = infer (arg_types @ env) subst e in
    let expr_type = construct_arrow_typ result_type arg_types in
    subst', apply_subst subst' expr_type
  | Lam (args, e) ->
    let arg_types = List.map (fun arg -> arg, fresh_type ()) args in
    let subst', result_type = infer (arg_types @ env) subst e in
    let expr_type = construct_arrow_typ result_type arg_types in
    subst', apply_subst subst' expr_type
  | App (e1, e2) ->
    let subst1, t1 = infer env subst e1 in
    let subst2, t2 = infer env subst1 e2 in
    let result_type = fresh_type () in
    let subst3 = unify subst2 t1 (TArrow (t2, result_type)) in
    subst3, apply_subst subst3 result_type
  | Prim (_, e1, e2) ->
    let subst1, t1 = infer env subst e1 in
    let subst2, t2 = infer env subst1 e2 in
    let subst3 = unify subst2 t1 TInt in
    let subst4 = unify subst3 t2 TInt in
    subst4, TInt
  | Let (x, e1, e2) ->
    let subst1, t1 = infer env subst e1 in
    let subst2, t2 = infer ((x, t1) :: env) subst1 e2 in
    subst2, t2
;;

(* Annotation *)
let rec annotate env subst expr =
  match expr with
  | Const (CInt i) -> subst, AConst (CInt i, TInt), (None, TInt)
  | Const (CBool b) -> subst, AConst (CBool b, TBool), (None, TBool)
  | Const CUnit ->
    subst, AConst (CUnit, TUnit), (None, TUnit) (* Desugar delay to a thunk *)
  | Delay e ->
    let subst', body_annot, (_, body_type) = annotate env subst e in
    ( subst'
    , ALam ([ "unit", TUnit ], body_annot, TArrow (TUnit, body_type))
    , (None, TArrow (TUnit, body_type)) )
  (* Desugar to forcing a thunk *)
  | Advance e ->
    let subst', thunk_body, (_, thunk_type) = annotate env subst e in
    subst', AApp (thunk_body, [ AConst (CUnit, TUnit) ], thunk_type), (None, thunk_type)
  | Var x ->
    let subst', t = infer env subst expr in
    subst', AVar (x, t), (None, t)
  | FunDef (name, args, body) ->
    let arg_types = List.map (fun arg -> arg, fresh_type ()) args in
    let subst', body_annot, (_, body_type) = annotate (arg_types @ env) subst body in
    let substituted_args =
      List.map (fun (arg, ty) -> arg, apply_subst subst' ty) arg_types
    in
    let def_type = construct_arrow_typ body_type arg_types |> apply_subst subst' in
    subst', AFunDef (name, substituted_args, body_annot, def_type), (Some name, def_type)
  | Lam (args, body) ->
    let arg_types = List.map (fun arg -> arg, fresh_type ()) args in
    let subst', body_annot, (_, body_type) = annotate (arg_types @ env) subst body in
    let substituted_args =
      List.map (fun (arg, ty) -> arg, apply_subst subst' ty) arg_types
    in
    let expr_type = construct_arrow_typ body_type arg_types in
    ( subst'
    , ALam (substituted_args, body_annot, body_type)
    , (None, apply_subst subst' expr_type) )
  | App (e1, e2) ->
    let subst1, annot1, (_, t1) = annotate env subst e1 in
    let subst2, annot2, (_, t2) = annotate env subst1 e2 in
    let result_type = fresh_type () in
    let subst3 = unify subst2 t1 (TArrow (t2, result_type)) in
    ( subst3
    , AApp (annot1, [ annot2 ], apply_subst subst3 result_type)
    , (None, apply_subst subst3 result_type) )
  | Prim (op, e1, e2) ->
    let subst1, annot1, (_, t1) = annotate env subst e1 in
    let subst2, annot2, (_, t2) = annotate env subst1 e2 in
    let subst3 = unify subst2 t1 TInt in
    let subst4 = unify subst3 t2 TInt in
    subst4, APrim (op, annot1, annot2, TInt), (None, TInt)
  | Let (x, e1, e2) ->
    let subst1, annot1, (_, t1) = annotate env subst e1 in
    let subst2, annot2, (_, t2) = annotate ((x, t1) :: env) subst1 e2 in
    subst2, ALet (x, t1, annot1, annot2), (Some x, apply_subst subst2 t2)
;;

let prepend_opt_binding env = function
  | None, _ -> env
  | Some name, typ -> (name, typ) :: env
;;

(* type annotate_meta =
  { subst : substitution
  ; env : (sym * typ) list
  ; annot_exprs : annot_expr list
  }*)

let rec expr_apply_subst subst expr =
  match expr with
  | AConst _ -> expr
  | AVar (x, t) -> AVar (x, apply_subst subst t)
  | AFunDef (name, args, body, t) ->
    AFunDef
      ( name
      , List.map (fun (arg, ty) -> arg, apply_subst subst ty) args
      , expr_apply_subst subst body
      , apply_subst subst t )
  | ALam (args, body, t) ->
    ALam
      ( List.map (fun (arg, ty) -> arg, apply_subst subst ty) args
      , expr_apply_subst subst body
      , apply_subst subst t )
  | AApp (e1, e2s, t) ->
    AApp
      ( expr_apply_subst subst e1
      , List.map (expr_apply_subst subst) e2s
      , apply_subst subst t )
  | APrim (op, e1, e2, t) ->
    APrim (op, expr_apply_subst subst e1, expr_apply_subst subst e2, apply_subst subst t)
  | ALet (x, ty, e1, e2) ->
    ALet (x, apply_subst subst ty, expr_apply_subst subst e1, expr_apply_subst subst e2)
;;

let annotate_all exprs =
  let env, subst, exprs =
    List.fold_left
      (fun (env, subst, annot_exprs) expr ->
        let subst', annot_expr, binding = annotate env subst expr in
        prepend_opt_binding env binding, subst', annot_expr :: annot_exprs)
      ([], [], [])
      exprs
  in
  let exprs' = List.map (expr_apply_subst subst) exprs in
  env, subst, exprs'
;;

(* Pretty printing *)
let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "()"
  | TVar n -> "t" ^ string_of_int n
  | TArrow (t1, t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
;;

let rec unfold_lam_args = function
  | [] -> ""
  | (name, typ) :: tail ->
    Printf.sprintf "(%s : %s) " name (string_of_type typ) ^ unfold_lam_args tail
;;

let rec string_of_annot_expr = function
  | AConst (CInt i, _) -> string_of_int i
  | AConst (CBool b, _) -> string_of_bool b
  | AConst (CUnit, _) -> "()"
  | AVar (name, typ) -> Printf.sprintf "%s : %s" name (string_of_type typ)
  | ALam (args, body, _) ->
    Printf.sprintf "(fun %s-> %s)" (unfold_lam_args args) (string_of_annot_expr body)
  | AApp (func, args, typ) ->
    Printf.sprintf
      "%s %s) : %s"
      (string_of_annot_expr func)
      (unfold_app_args args)
      (string_of_type typ)
  | APrim (op, e1, e2, _typ) ->
    Printf.sprintf
      "%s %s %s"
      (string_of_annot_expr e1)
      (string_of_binop op)
      (string_of_annot_expr e2)
  | AFunDef (name, args, body, typ) ->
    Printf.sprintf
      "def %s %s: %s = %s;"
      name
      (unfold_lam_args args)
      (string_of_type typ)
      (string_of_annot_expr body)
  | ALet (name, typ, rhs, body) ->
    Printf.sprintf
      "let (%s : %s) = %s in %s"
      name
      (string_of_type typ)
      (string_of_annot_expr rhs)
      (string_of_annot_expr body)

and unfold_app_args = function
  | [] -> ""
  | x :: xs -> Printf.sprintf "%s" (string_of_annot_expr x) ^ " " ^ unfold_app_args xs
;;
