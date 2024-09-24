module Environment = Map.Make (String)

type binop =
  | Add
  | Mul
  | Sub

type typ =
  | TInt
  | TArrow of typ * typ

type texpr =
  | CstI of int
  | Var of string
  | Lam of string * typ * texpr
  | App of texpr * texpr
  | Prim of binop * texpr * texpr

type value =
  | VInt of int
  | VClosure of string * texpr * value Environment.t

let rec interp x env =
  match x with
  | CstI i -> VInt i
  | Var v ->
    (match Environment.find_opt v env with
     | Some var -> var
     | None -> failwith "variable not defined in environment")
  | Lam (param, _, body) -> VClosure (param, body, env)
  | Prim (op, e1, e2) ->
    (match interp e1 env, interp e2 env with
     | VInt x, VInt y ->
       (match op with
        | Add -> VInt (x + y)
        | Mul -> VInt (x * y)
        | Sub -> VInt (x - y))
     | VClosure _, VClosure _ -> failwith "cannot do primitive operations on two closures"
     | _, VClosure _ -> failwith "cannot do primitive operations on int and closure"
     | VClosure _, _ -> failwith "cannot do primitive operations on closure and int")
  | App (body, arg') ->
    let arg = interp arg' env in
    (match interp body env with
     | VClosure (param, body', env') -> interp body' (Environment.add param arg env')
     | _ -> failwith "Cannot apply to non-closure")
;;

let id = Lam ("x", TInt, Var "x")
let empty : value Environment.t = Environment.empty
let env = Environment.add "y" (VInt 125) empty
let example = App (id, CstI 84)
let evaluated = interp example env
let add_example = Prim (Add, CstI 42, CstI 42)
let add_eval = interp add_example Environment.empty
let sub_example = Prim (Sub, CstI 42, CstI 42)
let sub_eval = interp sub_example Environment.empty
let square_fun = Lam ("x", TInt, Prim (Mul, Var "x", Var "x"))
let five_squared = interp (App (square_fun, CstI 5)) Environment.empty

let binop_with_defined_variable_does_not_fail =
  interp (Prim (Add, CstI 42, Var "x")) (Environment.add "x" (VInt 42) Environment.empty)
;;

let binop_with_undefined_variable_fails =
  interp (Prim (Add, CstI 42, Var "x")) (Environment.add "y" (VInt 42) Environment.empty)
;;

let binop_with_lambda_fails =
  interp (Prim (Add, CstI 42, Lam ("x", TInt, CstI 2))) Environment.empty
;;
