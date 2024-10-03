open Source

(** Symbol i.e. variables *)
type sym = string [@@deriving show]

module Environment = Map.Make (struct
    type t = sym

    let compare = String.compare
  end)

type typ =
  | TInt
  | TVar of int
  | TArrow of typ * typ
[@@deriving show]

(** Annotated expression (Types) *)
type annot_expr =
  | ACstI of int * typ
  | AVar of sym * typ
  | ALam of (sym * typ) list * annot_expr
  | AApp of annot_expr * annot_expr list * typ
  | APrim of binop * annot_expr * annot_expr * typ
  | ALet of sym * typ * annot_expr * annot_expr
[@@deriving show]

(* Type inference *)
let type_counter = ref 0

let fresh_type () =
  incr type_counter;
  TVar !type_counter
;;

type substitution = (int * typ) list

let rec apply_subst subst = function
  | TInt -> TInt
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
  | TInt -> false
;;

(* follows https://stanford-cs242.github.io/f19/lectures/02-2-type-systems partly*)
let rec infer env subst = function
  | CstI _ -> subst, TInt
  | Var x ->
    (try
       let t = List.assoc x env in
       subst, apply_subst subst t
     with
     | Not_found -> failwith ("unbound variable: " ^ x))
  | Lam (x, e) ->
    let arg_type = fresh_type () in
    let subst', result_type = infer ((x, arg_type) :: env) subst e in
    subst', TArrow (apply_subst subst' arg_type, result_type)
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
  | Var x ->
    let subst', t = infer env subst expr in
    subst', AVar (x, t), t
  | Lam (x, e) ->
    let arg_type = fresh_type () in
    let subst', body_annot, body_type = annotate ((x, arg_type) :: env) subst e in
    ( subst'
    , ALam ([ x, apply_subst subst' arg_type ], body_annot)
    , TArrow (apply_subst subst' arg_type, body_type) )
  | App (e1, e2) ->
    let subst1, annot1, t1 = annotate env subst e1 in
    let subst2, annot2, t2 = annotate env subst1 e2 in
    let result_type = fresh_type () in
    let subst3 = unify subst2 t1 (TArrow (t2, result_type)) in
    ( subst3
    , AApp (annot1, [annot2], apply_subst subst3 result_type)
    , apply_subst subst3 result_type )
  | CstI i -> subst, ACstI (i, TInt), TInt
  | Prim (op, e1, e2) ->
    let subst1, annot1, t1 = annotate env subst e1 in
    let subst2, annot2, t2 = annotate env subst1 e2 in
    let subst3 = unify subst2 t1 TInt in
    let subst4 = unify subst3 t2 TInt in
    subst4, APrim (op, annot1, annot2, TInt), TInt
  | Let (x, e1, e2) ->
    let subst1, annot1, t1 = annotate env subst e1 in
    let subst2, annot2, t2 = annotate ((x, t1) :: env) subst1 e2 in
    subst2, ALet (x, t1, annot1, annot2), t2
;;

(* Pretty printing *)
let rec string_of_type = function
  | TInt -> "int"
  | TVar n -> "t" ^ string_of_int n
  | TArrow (t1, t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
;;
