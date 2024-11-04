(* Common types shared across the implementation *)
type name = string

(* Unified expression type *)
type expr =
  | Var of name
  | Index of int (* de Bruijn index *)
  | Lambda of name * expr
  | App of expr * expr
  | Let of name * expr * expr
  | LetRec of name * expr * expr
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Op of string * expr * expr

(* Supporting type definitions *)
type value =
  | VInt of int
  | VBool of bool
  | VClosure of env * name * expr
  | VRecClosure of env * name * name * expr

and env = (name * value) list

module NameSet = Set.Make (String)
module NameMap = Map.Make (String)

(* Supercombinator with optimization flags *)
type supercombinator =
  { name : name
  ; params : name list
  ; body : expr
  ; is_wrapper : bool (* Flag for wrapper functions *)
  ; used_params : NameSet.t (* Actually used parameters *)
  }

(* Result type for lambda lifting *)
type lifting_result =
  { transformed_expr : expr
  ; new_supercombinators : supercombinator list
  }

(* Context for de Bruijn conversion *)
type context =
  { bound : name list
  ; free : name list
  ; depth : int
  }

let empty_context = { bound = []; free = []; depth = 0 }

(* Generate unique names for lifted functions *)
let name_generator =
  let counter = ref 0 in
  fun base ->
    incr counter;
    Printf.sprintf "%s_%d" base !counter
;;

(* Helper functions *)
let find_index name ctx =
  let rec loop n = function
    | [] -> None
    | x :: xs -> if x = name then Some n else loop (n + 1) xs
  in
  loop 0 ctx.bound
;;

let find_free_index name ctx =
  let rec loop n = function
    | [] -> None
    | x :: xs -> if x = name then Some n else loop (n + 1) xs
  in
  loop 0 ctx.free
;;

(* Convert to de Bruijn indices *)
let rec to_debruijn ctx expr =
  match expr with
  | Var x ->
    (match find_index x ctx with
     | Some n -> Index n
     | None ->
       (match find_free_index x ctx with
        | Some n -> Var x (* Keep free variables as named *)
        | None -> Var x))
    (* External reference *)
  | Lambda (x, body) ->
    let new_ctx = { bound = x :: ctx.bound; free = ctx.free; depth = ctx.depth + 1 } in
    Lambda (x, to_debruijn new_ctx body)
  | App (e1, e2) -> App (to_debruijn ctx e1, to_debruijn ctx e2)
  | Let (x, e1, e2) ->
    let e1' = to_debruijn ctx e1 in
    let new_ctx = { bound = x :: ctx.bound; free = ctx.free; depth = ctx.depth + 1 } in
    Let (x, e1', to_debruijn new_ctx e2)
  | LetRec (f, e1, e2) ->
    let new_ctx = { bound = f :: ctx.bound; free = ctx.free; depth = ctx.depth + 1 } in
    let e1' = to_debruijn new_ctx e1 in
    let e2' = to_debruijn new_ctx e2 in
    LetRec (f, e1', e2')
  | If (c, t, e) -> If (to_debruijn ctx c, to_debruijn ctx t, to_debruijn ctx e)
  | Op (op, e1, e2) -> Op (op, to_debruijn ctx e1, to_debruijn ctx e2)
  | e -> e
;;

(* Collect used variables *)
let rec collect_used_vars = function
  | Var x -> NameSet.singleton x
  | Index _ -> NameSet.empty
  | Lambda (_, body) -> collect_used_vars body
  | App (e1, e2) -> NameSet.union (collect_used_vars e1) (collect_used_vars e2)
  | Let (x, e1, e2) -> NameSet.union (collect_used_vars e1) (collect_used_vars e2)
  | LetRec (f, e1, e2) -> NameSet.union (collect_used_vars e1) (collect_used_vars e2)
  | If (c, t, e) ->
    NameSet.union
      (collect_used_vars c)
      (NameSet.union (collect_used_vars t) (collect_used_vars e))
  | Op (_, e1, e2) -> NameSet.union (collect_used_vars e1) (collect_used_vars e2)
  | _ -> NameSet.empty
;;

(* Get free variables in an expression *)
let rec free_vars = function
  | Var x -> NameSet.singleton x
  | Index _ -> NameSet.empty
  | Lambda (x, e) -> NameSet.remove x (free_vars e)
  | App (e1, e2) -> NameSet.union (free_vars e1) (free_vars e2)
  | Let (x, e1, e2) -> NameSet.union (free_vars e1) (NameSet.remove x (free_vars e2))
  | LetRec (f, e1, e2) ->
    NameSet.union (NameSet.remove f (free_vars e1)) (NameSet.remove f (free_vars e2))
  | If (e1, e2, e3) ->
    NameSet.union (free_vars e1) (NameSet.union (free_vars e2) (free_vars e3))
  | Op (_, e1, e2) -> NameSet.union (free_vars e1) (free_vars e2)
  | _ -> NameSet.empty
;;

(* Optimized lambda lifting *)
let rec optimize_lift (expr : expr) : lifting_result =
  match expr with
  | Lambda (param, body) ->
    let body_result = optimize_lift body in
    let free = NameSet.remove param (free_vars body) in
    let free_list = NameSet.elements free in
    if NameSet.is_empty free
    then
      (* No free variables - no need to lift *)
      { transformed_expr = Lambda (param, body_result.transformed_expr)
      ; new_supercombinators = body_result.new_supercombinators
      }
    else (
      (* Create optimized supercombinator *)
      let sc_name = name_generator "sc" in
      let used_vars = collect_used_vars body_result.transformed_expr in
      let optimized_params =
        free_list @ [ param ] |> List.filter (fun p -> NameSet.mem p used_vars)
      in
      let sc =
        { name = sc_name
        ; params = optimized_params
        ; body = to_debruijn empty_context body_result.transformed_expr
        ; is_wrapper = List.length optimized_params = 1
        ; used_params = used_vars
        }
      in
      (* Create optimized application *)
      let transformed =
        List.fold_left
          (fun acc free_var ->
            if NameSet.mem free_var used_vars then App (acc, Var free_var) else acc)
          (Var sc_name)
          free_list
      in
      { transformed_expr = transformed
      ; new_supercombinators = sc :: body_result.new_supercombinators
      })
  | Let (name, value, body) ->
    let value_result = optimize_lift value in
    let body_result = optimize_lift body in
    (* Optimize let-binding if possible *)
    let used_in_body = collect_used_vars body_result.transformed_expr in
    if not (NameSet.mem name used_in_body)
    then body_result (* Variable not used - eliminate let *)
    else
      { transformed_expr =
          Let (name, value_result.transformed_expr, body_result.transformed_expr)
      ; new_supercombinators =
          value_result.new_supercombinators @ body_result.new_supercombinators
      }
  | LetRec (name, value, body) as expr ->
    let value_result = optimize_lift value in
    let body_result = optimize_lift body in
    let used_vars = collect_used_vars expr in
    if not (NameSet.mem name used_vars)
    then (* Function is never used - can be eliminated *)
      body_result
    else
      { transformed_expr =
          LetRec (name, value_result.transformed_expr, body_result.transformed_expr)
      ; new_supercombinators =
          value_result.new_supercombinators @ body_result.new_supercombinators
      }
  | App (e1, e2) ->
    let e1_result = optimize_lift e1 in
    let e2_result = optimize_lift e2 in
    { transformed_expr = App (e1_result.transformed_expr, e2_result.transformed_expr)
    ; new_supercombinators =
        e1_result.new_supercombinators @ e2_result.new_supercombinators
    }
  | If (cond, then_expr, else_expr) ->
    let cond_result = optimize_lift cond in
    let then_result = optimize_lift then_expr in
    let else_result = optimize_lift else_expr in
    { transformed_expr =
        If
          ( cond_result.transformed_expr
          , then_result.transformed_expr
          , else_result.transformed_expr )
    ; new_supercombinators =
        cond_result.new_supercombinators
        @ then_result.new_supercombinators
        @ else_result.new_supercombinators
    }
  | Op (op, e1, e2) ->
    let e1_result = optimize_lift e1 in
    let e2_result = optimize_lift e2 in
    { transformed_expr = Op (op, e1_result.transformed_expr, e2_result.transformed_expr)
    ; new_supercombinators =
        e1_result.new_supercombinators @ e2_result.new_supercombinators
    }
  | expr -> { transformed_expr = expr; new_supercombinators = [] }
;;

(* Enhanced pretty printing *)
let rec string_of_expr = function
  | Var x -> x
  | Index n -> Printf.sprintf "[%d]" n
  | Lambda (x, e) -> Printf.sprintf "(λ%s.%s)" x (string_of_expr e)
  | App (e1, e2) -> Printf.sprintf "(%s %s)" (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2) ->
    Printf.sprintf "let %s = %s in %s" x (string_of_expr e1) (string_of_expr e2)
  | LetRec (x, e1, e2) ->
    Printf.sprintf "let rec %s = %s in %s" x (string_of_expr e1) (string_of_expr e2)
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | If (c, t, e) ->
    Printf.sprintf
      "if %s then %s else %s"
      (string_of_expr c)
      (string_of_expr t)
      (string_of_expr e)
  | Op (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) op (string_of_expr e2)
;;

let string_of_supercombinator sc =
  Printf.sprintf
    "%s %s = %s%s"
    sc.name
    (String.concat " " sc.params)
    (string_of_expr sc.body)
    (if sc.is_wrapper then " (wrapper)" else "")
;;

(* Test function *)
let test_lambda_lifting expr =
  let result = optimize_lift expr in
  Printf.printf "Transformed expression:\n%s\n\n" (string_of_expr result.transformed_expr);
  Printf.printf "Generated supercombinators:\n";
  List.iter
    (fun sc -> Printf.printf "%s\n" (string_of_supercombinator sc))
    result.new_supercombinators
;;

let print_separator () = print_endline "\n----------------------------------------\n"

let () =
  (* Example 1: Simple lambda with free variables *)
  let example1 =
    Let
      ( "x"
      , Int 10
      , Let ("y", Int 5, Lambda ("z", Op ("+", Op ("+", Var "x", Var "y"), Var "z"))) )
  in
  print_endline "Example 1: Lambda with free variables";
  print_endline "Original expression:";
  print_endline (string_of_expr example1);
  print_separator ();
  test_lambda_lifting example1;
  print_separator ();
  let example2 =
    Lambda
      ( "x"
      , Lambda
          ( "y"
          , Lambda
              ("z", If (Op ("<", Var "x", Var "y"), Var "z", Op ("+", Var "x", Var "y")))
          ) )
  in
  print_endline "Example 2: Nested lambdas";
  print_endline "Original expression:";
  print_endline (string_of_expr example2);
  print_separator ();
  test_lambda_lifting example2;
  print_separator ();
  (* Example 3: Recursive factorial function *)
  let example3 =
    LetRec
      ( "fact"
      , Lambda
          ( "n"
          , If
              ( Op ("=", Var "n", Int 0)
              , Int 1
              , Op ("*", Var "n", App (Var "fact", Op ("-", Var "n", Int 1))) ) )
      , App (Var "fact", Int 5) )
  in
  print_endline "Example 3: Recursive factorial";
  print_endline "Original expression:";
  print_endline (string_of_expr example3);
  print_separator ();
  test_lambda_lifting example3;
  print_separator ();
  (* Example 4: Higher-order function (map-like) *)
  let example4 =
    Let
      ( "double"
      , Lambda ("x", Op ("*", Var "x", Int 2))
      , Let
          ( "apply_to_two"
          , Lambda ("f", App (Var "f", Int 2))
          , App (Var "apply_to_two", Var "double") ) )
  in
  print_endline "Example 4: Higher-order function";
  print_endline "Original expression:";
  print_endline (string_of_expr example4);
  print_separator ();
  test_lambda_lifting example4;
  print_separator ()
;;
