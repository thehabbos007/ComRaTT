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

let id = ALam ("x", TInt, AVar ("x", TInt))
let empty : value Environment.t = Environment.empty
let env = Environment.add "y" (VInt 125) empty
let example = AApp (id, ACstI (84, TInt), TInt)
let evaluated = interp example env
let add_example = APrim (Add, ACstI (42, TInt), ACstI (42, TInt), TInt)
let add_eval = interp add_example Environment.empty
let sub_example = APrim (Sub, ACstI (42, TInt), ACstI (42, TInt), TInt)
let sub_eval = interp sub_example Environment.empty
let square_fun = ALam ("x", TInt, APrim (Mul, AVar ("x", TInt), AVar ("x", TInt), TInt))
let five_squared = interp (AApp (square_fun, ACstI (5, TInt), TInt)) Environment.empty

let binop_with_defined_variable_does_not_fail =
  interp
    (APrim (Add, ACstI (42, TInt), AVar ("x", TInt), TInt))
    (Environment.add "x" (VInt 42) Environment.empty)
;;

let binop_with_undefined_variable_fails =
  interp
    (APrim (Add, ACstI (42, TInt), AVar ("x", TInt), TInt))
    (Environment.add "y" (VInt 42) Environment.empty)
;;

let binop_with_lambda_fails =
  interp
    (APrim (Add, ACstI (42, TInt), ALam ("x", TInt, ACstI (2, TInt)), TInt))
    Environment.empty
;;
