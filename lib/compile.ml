open Annotate

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
      AApp (lambda, List.map (fun (x, t) -> AVar (x, t)) free_vars, TInt)
    | AApp (e1, e2, t) -> AApp (convert e1, List.map convert e2, t)
    | APrim (op, e1, e2, t) -> APrim (op, convert e1, convert e2, t)
    | ALet (x, t, e1, e2) -> ALet (x, t, convert e1, convert e2)
    | e -> e
  in
  convert expr
;;
;;
