open Annotate

type global_def =
  { name : sym
  ; args : (sym * typ) list
  ; body : annot_expr
  ; ret_type : typ
  }

(* Internal definitions, invisible to importers *)
open struct
  let name_counter = ref 0

  let unique_name x =
    incr name_counter;
    Printf.sprintf "#%s_%d" x !name_counter
  ;;

  let rec free_vars = function
    | ACstI _ -> []
    | AVar (x, t) -> [ x, t ]
    | ALam (params, body, _) ->
      let param_names = List.map fst params in
      List.filter (fun (x, _) -> not (List.mem x param_names)) (free_vars body)
    | AApp (e1, e2, _) ->
      List.sort_uniq compare (free_vars e1 @ (List.map free_vars e2 |> List.concat))
    | APrim (_, e1, e2, _) -> List.sort_uniq compare (free_vars e1 @ free_vars e2)
    | ALet (x, t, e1, e2) ->
      let fv1 = free_vars e1 in
      let fv2 = List.filter (( <> ) (x, t)) (free_vars e2) in
      List.sort_uniq compare (fv1 @ fv2)
  ;;

  let convert_params_to_arrow params ret_type =
    List.fold_right (fun (_, t) acc -> TArrow (t, acc)) params ret_type
  ;;

  let closure_convert globals expr =
    let rec convert = function
      | ALet (x, t1, ALam (params, body, t2), e2) ->
        ALet (x, t1, ALam (params, convert body, t2), convert e2)
      | ALam (params, body, t) as e ->
        let free_vars =
          List.filter
            (fun (v, _) -> not (List.mem v globals || List.mem v (List.map fst params)))
            (free_vars e)
        in
        let new_params = free_vars @ params in
        let converted_body = convert body in
        let lambda = ALam (new_params, converted_body, t) in
        let substitute_type = convert_params_to_arrow params t in
        AApp (lambda, List.map (fun (x, t) -> AVar (x, t)) free_vars, substitute_type)
      | AApp (e1, e2, t) -> AApp (convert e1, List.map convert e2, t)
      | APrim (op, e1, e2, t) -> APrim (op, convert e1, convert e2, t)
      | ALet (x, t, e1, e2) -> ALet (x, t, convert e1, convert e2)
      | e -> e
    in
    convert expr
  ;;

  let lift_lambdas expr =
    let definitions = ref [] in
    let rec lift = function
      (* Important pattern: keep top-level lambda bindings,
         these are user-defined top-levelfunctions *)
      | ALet (x, t1, ALam (params, body, t2), e2) ->
        ALet (x, t1, ALam (params, lift body, t2), lift e2)
      | ALam (params, body, ret_type) ->
        let lifted_body = lift body in
        let name = unique_name "global_lam" in
        definitions
        := { name; args = params; body = lifted_body; ret_type } :: !definitions;
        AVar (name, ret_type)
      | AApp (e1, e2, t) -> AApp (lift e1, List.map lift e2, t)
      | APrim (op, e1, e2, t) -> APrim (op, lift e1, lift e2, t)
      | ALet (x, t, e1, e2) -> ALet (x, t, lift e1, lift e2)
      | e -> e
    in
    let lifted_expr = lift expr in
    lifted_expr, !definitions
  ;;
end

module Lift = struct
  let lambda_lift_expr globals expr =
    let closed_expr = closure_convert globals expr in
    lift_lambdas closed_expr
  ;;
end

module ConstantFold = struct
  (* Fold constants into expression that can be eliminated *)
  let rec constant_fold_expr expr =
    match expr with
    | ACstI _ -> expr
    | AVar _ -> expr
    | APrim (Add, ACstI (n1, TInt), ACstI (n2, TInt), TInt) -> ACstI (n1 + n2, TInt)
    | APrim (Sub, ACstI (n1, TInt), ACstI (n2, TInt), TInt) -> ACstI (n1 - n2, TInt)
    | APrim (Mul, ACstI (n1, TInt), ACstI (n2, TInt), TInt) -> ACstI (n1 * n2, TInt)
    | APrim (op, e1, e2, t) -> APrim (op, constant_fold_expr e1, constant_fold_expr e2, t)
    | ALet (x, t, e1, e2) -> ALet (x, t, constant_fold_expr e1, constant_fold_expr e2)
    | AApp (e1, e2, t) -> AApp (constant_fold_expr e1, List.map constant_fold_expr e2, t)
    | ALam (params, body, t) -> ALam (params, constant_fold_expr body, t)
  ;;
end

let list_take n list = 
  let rec aux n acc = function
  | [] -> None
  | _ when n = 0 -> Some (List.rev acc)
  | x::xs -> aux (n-1) (x :: acc) xs
  in aux n [] list

module EliminatePartialApp = struct
  let rec unpack_type ty = 
    match ty with
    | TInt -> []
    | TVar _ -> []
    | TArrow (_t1, _t2) -> failwith "what to do"

  let rec eliminate_partial aexpr = 
    match aexpr with
    | ACstI _ -> aexpr
    | AVar (_name, TArrow(_t1, _t2)) -> aexpr
    | AVar _ -> aexpr
    | APrim (op, e1, e2, ty) -> APrim (op, eliminate_partial e1, eliminate_partial e2, ty)
    | ALam (args, body, ty) -> ALam (args, eliminate_partial body, ty)
    | AApp (func, args, _ty) -> 
      let _transformed_func = eliminate_partial func in
      let _transformed_args = List.map eliminate_partial args in
      aexpr
    | ALet (_name, _ty, AVar (_x, TArrow(t1, t2)), _cont) -> aexpr
    | _ -> failwith "bananer i pyjamas"
    
  (*
  let rec eliminate_partial aexpr =
    match aexpr with
    (* No partial app in int constants *)
    | ACstI _ -> aexpr 
    (* No partial app in vars *)
    | AVar _ -> aexpr 
    (* For arithmetic operations, just call recursively on expressions *)
    | APrim (op, e1, e2, ty) -> APrim(op, eliminate_partial e1, eliminate_partial e2, ty)
    (* For lambdas we need to call recursively in the body *)
    | ALam (args, body, ty) -> 
      let transformed_body = eliminate_partial body in
      ALam (args, transformed_body, ty)
    (* For function applications we need to call recursively in both body/function and args
       We also need to check whether the list of args matches the expected args
       of a lambda
    *)
    | AApp (func, args, _ty) -> 
      let transformed_func = eliminate_partial func in
      let transformed_args = List.map eliminate_partial args in
      (match transformed_func with

        (* The case where total application is possible. Just apply, right? *)
        | ALam(lArgs, lBody, lTy) when List.length transformed_args == List.length lArgs ->
          AApp(lBody, transformed_args, lTy)

        (* The cases where total application is not possible. What to do? 
          It must depend on the type of the lambda.
          If we have more args than we want and the lambda is a TArrow,
          then we should be able to apply the correct amount of args
          and then recursively check and apply, right?

          If we have more args than we want and the type is not TArrow,
          then we have a problem.

           
        *)
        (* More args than expected and arrow type of lambda 
          Should this actually do a recursive call again?
          TODO: har jeg misforstået hvad vi bruger tarrow til? en lambda fra int til int er vel en tarrow(tint, tint), og jeg har gået og bildt mig ind at det blot er int..
           
        *)
        | ALam (lArgs, lBody, TArrow (_t1, t2)) when List.length transformed_args > List.length lArgs ->
          (match list_take (List.length lArgs) transformed_args with
           | None -> failwith "not enough args"
           | Some args' -> AApp(lBody, args', t2) (* Naive: use return type of TArrow *)
          )
        | ALam _ -> failwith "total app not possible"

        (* Not a lambda. Do we support this? 
          If we do, then just AApp like nothing happened
        *)
        | _ -> failwith "what to do"
      )
    (* For let bindings, call recursively in both rhs and body *)
    | ALet (x, ty, rhs, body) -> ALet (x, ty, eliminate_partial rhs, eliminate_partial body)
    *)

end

(* just for testing the partial app elimination outside of this module*)
let part_elim expr = EliminatePartialApp.eliminate_partial expr

let optimize expr =
  let expr = ConstantFold.constant_fold_expr expr in
  Lift.lambda_lift_expr [] expr
;;