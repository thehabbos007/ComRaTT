type binop =
  | Add
  | Mul
  | Sub

type expr =
  | CstI of int
  | Var of string
  | Lam of string * expr
  | App of expr * expr
  | Prim of binop * expr * expr

type typ =
  | TInt
  | TVar of int
  | TArrow of typ * typ

(** Symbol i.e. variables *)
type sym = string

module Environment = Map.Make (struct
    type t = sym

    let compare = String.compare
  end)

(** Annotated expression (Types) *)
type annot_expr =
  | ACstI of int * typ
  | AVar of sym * typ
  | ALam of sym * typ * annot_expr
  | AApp of annot_expr * annot_expr * typ
  | APrim of binop * annot_expr * annot_expr * typ
  | ALet of sym * typ * annot_expr * annot_expr

type value =
  | VInt of int
  | VClosure of string * annot_expr * value Environment.t

let rec interp (x : annot_expr) env =
  match x with
  | ACstI (i, _) -> VInt i
  | AVar (v, _) ->
    (match Environment.find_opt v env with
     | Some var -> var
     | None -> failwith "variable not defined in environment")
  | ALam (param, _, body) -> VClosure (param, body, env)
  | APrim (op, e1, e2, _) ->
    (match interp e1 env, interp e2 env with
     | VInt x, VInt y ->
       (match op with
        | Add -> VInt (x + y)
        | Mul -> VInt (x * y)
        | Sub -> VInt (x - y))
     | VClosure _, VClosure _ -> failwith "cannot do primitive operations on two closures"
     | _, VClosure _ -> failwith "cannot do primitive operations on int and closure"
     | VClosure _, _ -> failwith "cannot do primitive operations on closure and int")
  | AApp (body, arg', _) ->
    let arg = interp arg' env in
    (match interp body env with
     | VClosure (param, body', env') -> interp body' (Environment.add param arg env')
     | _ -> failwith "Cannot apply to non-closure")
  | ALet (name, _, e1, e2) ->
    let e1' = interp e1 env in
    let env' = Environment.add name e1' env in
    interp e2 env'
;;

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
    , ALam (x, apply_subst subst' arg_type, body_annot)
    , TArrow (apply_subst subst' arg_type, body_type) )
  | App (e1, e2) ->
    let subst1, annot1, t1 = annotate env subst e1 in
    let subst2, annot2, t2 = annotate env subst1 e2 in
    let result_type = fresh_type () in
    let subst3 = unify subst2 t1 (TArrow (t2, result_type)) in
    ( subst3
    , AApp (annot1, annot2, apply_subst subst3 result_type)
    , apply_subst subst3 result_type )
  | CstI i -> subst, ACstI (i, TInt), TInt
  | Prim (op, e1, e2) ->
    let subst1, annot1, t1 = annotate env subst e1 in
    let subst2, annot2, t2 = annotate env subst1 e2 in
    let subst3 = unify subst2 t1 TInt in
    let subst4 = unify subst3 t2 TInt in
    subst4, APrim (op, annot1, annot2, TInt), TInt
;;

let run_example ast ~(env : value Environment.t) =
  let _, annotated, _ = annotate [ "x", TInt ] [] ast in
  interp annotated env
;;

let id = Lam ("x", Var "x")
let square_fun = Lam ("x", Prim (Mul, Var "x", Var "x"))
let example = App (id, CstI 84) |> run_example ~env:Environment.empty
let add_example = Prim (Add, CstI 42, CstI 42) |> run_example ~env:Environment.empty
let sub_example = Prim (Sub, CstI 42, CstI 42) |> run_example ~env:Environment.empty

let binop_with_defined_variable_does_not_fail =
  run_example
    (Prim (Add, CstI 42, Var "x"))
    ~env:(Environment.add "x" (VInt 42) Environment.empty)
;;

let binop_with_lambda_fails = run_example (Prim (Add, CstI 42, Lam ("x", CstI 2)))
let five_squared = App (square_fun, CstI 5) |> run_example ~env:Environment.empty

let binop_with_undefined_variable_fails =
  run_example
    (Prim (Add, CstI 42, Var "x"))
    ~env:(Environment.add "y" (VInt 42) Environment.empty)
;;
