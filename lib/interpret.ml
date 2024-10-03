open Source
open Annotate

type value =
  | VInt of int
  | VClosure of string list * annot_expr * value Environment.t

let string_of_value = function
  | VInt i -> string_of_int i
  | VClosure (names, _, _) -> List.fold_left (fun acc name -> acc ^ name)"Lambda: " names
;;

let rec interp (x : annot_expr) env =
  match x with
  | ACstI (i, _) -> VInt i
  | AVar (v, _) ->
    (match Environment.find_opt v env with
     | Some var -> var
     | None -> failwith ("variable not defined in environment: " ^ v))
  | ALam ([ (param, _) ], body) -> VClosure ([param], body, env)
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
  | AApp (body, args', _) -> 
    let args = List.map (fun arg' -> interp arg' env) args' in
    (match interp body env with
     | VClosure (params, body', env') ->
      let zipped = List.combine params args in
      let env'' = List.fold_left (fun acc (param, arg) -> Environment.add param arg acc) env' zipped in
      interp body' env''

     | _ -> failwith "Cannot apply to non-closure")
  | ALet (name, _, e1, e2) ->
    let e1' = interp e1 env in
    let env' = Environment.add name e1' env in
    interp e2 env'
  | _ -> failwith "oopsie, applied more than one lambda arg.."
;;
