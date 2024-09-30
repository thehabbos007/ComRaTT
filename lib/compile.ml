open Annotate

let name_counter = ref 0

let unique_name x =
  incr name_counter;
  "#" ^ x ^ string_of_int !name_counter
;;

(** Step 1, rename all lambdas to a unique identifier*)
let rec lambda_lift_rename annot =
  match annot with
  | ACstI _ -> annot
  | AVar _ -> annot
  | ALam (args, body) -> ALam (args, lambda_lift_rename body)
  | APrim (op, e1, e2, t) -> APrim (op, lambda_lift_rename e1, lambda_lift_rename e2, t)
  | AApp (e1, e2, t) -> AApp (lambda_lift_rename e1, lambda_lift_rename e2, t)
  | ALet (x, t, (ALam _ as e1), e2) ->
    let new_name = unique_name x in
    let e2' = lambda_lift_rename e2 in
    ALet (new_name, t, lambda_lift_rename e1, rename_call e2' x new_name)
  | ALet (x, t, e1, e2) -> ALet (x, t, lambda_lift_rename e1, lambda_lift_rename e2)

and rename_call annot old_name new_name =
  match annot with
  | ACstI _ -> annot
  | AVar (x, t) -> if x = old_name then AVar (new_name, t) else annot
  | ALam (args, body) -> ALam (args, rename_call body old_name new_name)
  | APrim (op, e1, e2, t) ->
    APrim (op, rename_call e1 old_name new_name, rename_call e2 old_name new_name, t)
  | AApp (e1, e2, t) ->
    AApp (rename_call e1 old_name new_name, rename_call e2 old_name new_name, t)
  | ALet (x, t, e1, e2) ->
    ALet (x, t, rename_call e1 old_name new_name, rename_call e2 old_name new_name)
;;

(** Step 2, name anonymous lambdas*)
let rec lambda_lift_anon_names annot =
  match annot with
  | ACstI _ -> annot
  | AVar _ -> annot
  | ALam (args, body) -> failwith "not impl"
  | APrim (op, e1, e2, t) ->
    APrim (op, lambda_lift_anon_names e1, lambda_lift_anon_names e2, t)
  | AApp (e1, e2, t) -> AApp (lambda_lift_anon_names e1, lambda_lift_anon_names e2, t)
  | ALet (x, t, e1, e2) ->
    ALet (x, t, lambda_lift_anon_names e1, lambda_lift_anon_names e2)
;;
