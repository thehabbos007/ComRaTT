open Source
open Annotate

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VClosure of string list * annot_expr * value Environment.t

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VClosure (names, _, _) -> List.fold_left (fun acc name -> acc ^ name) "Lambda: " names
;;

let rec interp (x : annot_expr) env =
  match x with
  | AConst (CInt i, _) -> VInt i
  | AConst (CBool b, _) -> VBool b
  | AVar (v, _) ->
    (match Environment.find_opt v env with
     | Some var -> var
     | None -> failwith ("variable not defined in environment: " ^ v))
  | ALam ([ (param, _) ], body, _) -> VClosure ([ param ], body, env)
  | APrim (op, e1, e2, _) ->
    (match interp e1 env, interp e2 env with
     | VInt x, VInt y ->
       (match op with
        | Add -> VInt (x + y)
        | Mul -> VInt (x * y)
        | Sub -> VInt (x - y)
        | Eq -> VBool (x = y)
        | Lt -> VBool (x < y)
        | Lte -> VBool (x <= y)
        | Gt -> VBool (x > y)
        | Gte -> VBool (x >= y)
        | Neq -> VBool (x <> y))
     | _, VClosure _ ->
       failwith "cannot do primitive operations on anything and a closure"
     | VClosure _, _ -> failwith "cannot do primitive operations on closure and anything"
     | VBool _, _ -> failwith "cannot do primitive operations on bool and anything"
     | _, VBool _ -> failwith "cannot do primitive operations on anything and bool"
     | VUnit, _ -> failwith "cannot do primitive operations on unit and anything"
     | _, VUnit -> failwith "cannot do primitive operations on anything and unit")
  | AApp (body, args, _) ->
    let args' = List.map (fun arg' -> interp arg' env) args in
    (match interp body env with
     | VClosure (params, body', env') ->
       let zipped = List.combine params args' in
       let env'' =
         List.fold_left
           (fun acc (param, arg) -> Environment.add param arg acc)
           env'
           zipped
       in
       interp body' env''
     | _ -> failwith "Cannot apply to non-closure")
  | ALet (name, _, e1, e2) ->
    let e1' = interp e1 env in
    let env' = Environment.add name e1' env in
    interp e2 env'
  | AIfThenElse (guard, _, then_branch, else_branch, _) ->
    let guard_holds = interp guard env in
    (match guard_holds with
     | VBool t -> if t then interp then_branch env else interp else_branch env
     | VInt i -> if i <> 0 then interp then_branch env else interp else_branch env
     (* closures? *)
     | _ -> failwith "interp: if-guard not valid")
  (* This might not work in all cases *)
  | AFunDef (_name, _args, body, _) -> interp body env
  | _ -> failwith "Oops, unhandled or invalid annot_expr in interp"
;;
