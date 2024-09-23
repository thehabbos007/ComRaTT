module Environment = Map.Make (String)

type typ =
  | TInt
  | TArrow of typ * typ

type texpr =
  | CstI of int
  | Var of string
  | Lam of string * typ * texpr
  | App of texpr * texpr

type value = 
  VInt of int
  | VClosure of string * texpr * value Environment.t

let rec lookup env x = 
  match env with
  | [] -> failwith "var not found"
  | (var, value)::rest -> if var=x then value else lookup rest x

let rec interp x env =
  match x with
  | CstI i -> VInt i
  | Var v -> Environment.find v env
  | Lam(param, _, body) -> VClosure(param,body, env)
  | App(body, arg') -> let arg = interp arg' env in 
      match interp body env with
      | VClosure(param, body', env') -> interp body' (Environment.add param arg env')
      | _ -> failwith "No" 