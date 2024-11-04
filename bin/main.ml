type binop =
  | Add
  | Mul
  | Sub
[@@deriving show, eq]

type const =
  | CInt of int
  | CBool of bool
  | CUnit
[@@deriving show, eq]

type typ =
  | TInt
  | TBool
  | TUnit
  | TVar of int
  | TArrow of typ * typ
[@@deriving show, eq]

type t_expr =
  | TConst of const * typ
  | TVar of string * typ
  (* TODO: Consider top-level let bindings (top-level constants that are evaluated at the start of the program)
     One thing we have to consider is if we should handle functions that refer to top-level let bindings.
     When we introduce delay/adv, the top level bindings may need to be allocated in the heap at the start of the program.
  *)
  | TLam of (string * typ) list * t_expr * typ
  | TFunDef of string * (string * typ) list * t_expr * typ
  | TApp of t_expr * t_expr list * typ
  | TPrim of binop * t_expr * t_expr * typ
  | TLet of string * typ * t_expr * t_expr
[@@deriving show, eq]

(* lambda_lifted.ml *)
type llexpr =
  | LLConst of const
  | LLVar of string
  | LLLam of (string * typ) list * llexpr (* No closures yet *)
  | LLApp of llexpr * llexpr list
  | LLPrim of binop * llexpr * llexpr
  | LLLet of string * llexpr * llexpr
  | LLFunDef of string * (string * typ) list * llexpr
[@@deriving show, eq]

(* lambda_anf.ml *)
type immexpr =
  | ImmConst of const
  | ImmVar of string
[@@deriving show, eq]

type anfexpr =
  | ANFImm of immexpr
  | ANFPrim of binop * immexpr * immexpr
  | ANFLet of string * anfexpr * anfexpr
  | ANFApp of string * immexpr list (* Only named functions *)
[@@deriving show, eq]

type anf_function =
  { name : string
  ; args : (string * typ) list
  ; body : anfexpr
  }
[@@deriving show, eq]

type anf_program =
  { functions : anf_function list
  ; main : anfexpr
  }
[@@deriving show, eq]

type wasm_typ =
  | WInt32
  | WInt64
[@@deriving show, eq]

type wasm_instr =
  | WConst of int * wasm_typ
  | WGet_local of int
  | WSet_local of int
  | WCall of string
  | WAdd
  | WMul
  | WSub
  | WReturn
[@@deriving show, eq]

type wasm_function =
  { name : string
  ; params : (string * typ) list
  ; locals : int
  ; body : wasm_instr list
  }
[@@deriving show, eq]

type wasm_module = { functions : wasm_function list } [@@deriving show, eq]

(* lambda_lift.ml *)
let fresh_var =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Printf.sprintf "_v%d" !counter
;;

module Arg = struct
  type t = string * typ

  let compare (s1, t1) (s2, t2) =
    match compare s1 s2 with
    | 0 -> compare t1 t2
    | n -> n
  ;;
end

module SymSet = Set.Make (Arg)

let rec free_vars t_expr =
  match t_expr with
  | TConst (_, _) -> SymSet.empty
  | TVar (x, t) -> SymSet.singleton (x, t)
  | TLam (args, body, _) ->
    let arg_set = List.fold_left (fun set x -> SymSet.add x set) SymSet.empty args in
    SymSet.diff (free_vars body) arg_set
  | TApp (f, args, _) ->
    List.fold_left (fun set arg -> SymSet.union set (free_vars arg)) (free_vars f) args
  | TPrim (_, e1, e2, _) -> SymSet.union (free_vars e1) (free_vars e2)
  | TLet (x, t, e1, e2) ->
    SymSet.union (free_vars e1) (SymSet.remove (x, t) (free_vars e2))
  | TFunDef (name, args, body, t) ->
    let arg_set = List.fold_left (fun set x -> SymSet.add x set) SymSet.empty args in
    SymSet.diff (free_vars body) (SymSet.add (name, t) arg_set)
;;

let lift_lambdas expr =
  let functions = ref [] in
  let env = Hashtbl.create 32 in
  (* Track function name bindings *)
  let rec lift = function
    | TConst (c, _) -> LLConst c
    | TVar (x, _) ->
      (* Check if this variable refers to a function *)
      (try LLVar (Hashtbl.find env x) with
       | Not_found -> LLVar x)
    | TLam (args, body, _) ->
      let name = fresh_var () in
      let free = free_vars (TLam (args, body, TUnit)) in
      let lifted_body = lift body in
      functions := LLFunDef (name, args @ SymSet.elements free, lifted_body) :: !functions;
      let _ = Hashtbl.add env name name in
      (* Track this function *)
      LLVar name
    | TApp (f, args, _) ->
      let lifted_f = lift f in
      let lifted_args = List.map lift args in
      (match lifted_f with
       | LLVar fname -> LLApp (LLVar fname, lifted_args)
       | other ->
         let temp = fresh_var () in
         LLLet (temp, other, LLApp (LLVar temp, lifted_args)))
    | TPrim (op, e1, e2, _) -> LLPrim (op, lift e1, lift e2)
    | TLet (x, _, e1, e2) ->
      let lifted_e1 = lift e1 in
      (match lifted_e1 with
       | LLVar fname ->
         (* If binding a function, remember the mapping *)
         let _ = Hashtbl.add env x fname in
         LLLet (x, lifted_e1, lift e2)
       | other -> LLLet (x, other, lift e2))
    | TFunDef (name, args, body, _) ->
      let lifted_body = lift body in
      functions := LLFunDef (name, args, lifted_body) :: !functions;
      let _ = Hashtbl.add env name name in
      LLVar name
  in
  let main = lift expr in
  !functions, main
;;

(* anf.ml *)
let fresh_var =
  let counter = ref 0 in
  fun () ->
    incr counter;
    Printf.sprintf "_anf%d" !counter
;;

let gensym =
  let counter = ref 0 in
  fun prefix ->
    incr counter;
    prefix ^ string_of_int !counter
;;

let rec is_atomic = function
  | LLConst _ -> true
  | LLVar _ -> true
  | _ -> false
;;

let rec normalize_term exp = normalize exp (fun x -> x)

and normalize exp k =
  match exp with
  | LLLam (params, body) -> k (LLLam (params, normalize_term body))
  | LLLet (x, exp1, exp2) ->
    normalize exp1 (fun aexp1 -> LLLet (x, aexp1, normalize_term exp2))
  | LLPrim (op, exp1, exp2) ->
    normalize_name exp1 (fun t1 ->
      normalize_name exp2 (fun t2 -> k (LLPrim (op, t1, t2))))
  | LLApp (f, args) ->
    normalize_name f (fun t -> normalize_names args (fun ts -> k (LLApp (t, ts))))
  | exp when is_atomic exp -> k exp
  | _ -> failwith "Unexpected expression in normalization"

and normalize_name exp k =
  normalize exp (fun aexp ->
    if is_atomic aexp
    then k aexp
    else (
      let t = gensym "t" in
      LLLet (t, aexp, k (LLVar t))))

and normalize_names exps k =
  match exps with
  | [] -> k []
  | exp :: rest ->
    normalize_name exp (fun t -> normalize_names rest (fun ts -> k (t :: ts)))
;;

let convert_to_anf ll_expr =
  let functions = ref [] in
  let fresh_name =
    let counter = ref 0 in
    fun () -> Printf.sprintf "_fn%d" !counter
  in
  let rec convert = function
    | LLConst c -> ANFImm (ImmConst c)
    | LLVar x -> ANFImm (ImmVar x)
    | LLLam (params, body) ->
      let fn_name = fresh_name () in
      functions := (fn_name, params, convert body) :: !functions;
      ANFImm (ImmVar fn_name)
    | LLPrim (op, e1, e2) ->
      (match e1, e2 with
       | LLVar x, LLVar y -> ANFPrim (op, ImmVar x, ImmVar y)
       | LLConst c, LLVar y -> ANFPrim (op, ImmConst c, ImmVar y)
       | LLVar x, LLConst c -> ANFPrim (op, ImmVar x, ImmConst c)
       | LLConst c1, LLConst c2 -> ANFPrim (op, ImmConst c1, ImmConst c2)
       | _, _ ->
         let t1 = gensym "t" in
         let t2 = gensym "t" in
         ANFLet
           (t1, convert e1, ANFLet (t2, convert e2, ANFPrim (op, ImmVar t1, ImmVar t2))))
    | LLLet (x, e1, e2) -> ANFLet (x, convert e1, convert e2)
    | LLApp (LLVar f, args) ->
      let rec convert_args = function
        | [] -> [], []
        | arg :: rest ->
          let rest_vars, rest_bindings = convert_args rest in
          (match arg with
           | LLVar x -> ImmVar x :: rest_vars, rest_bindings
           | LLConst c -> ImmConst c :: rest_vars, rest_bindings
           | _ ->
             let t = gensym "t" in
             let converted = convert arg in
             ImmVar t :: rest_vars, (t, converted) :: rest_bindings)
      in
      let arg_vars, bindings = convert_args args in
      List.fold_right
        (fun (var, expr) acc -> ANFLet (var, expr, acc))
        bindings
        (ANFApp (f, arg_vars))
    | LLApp (f, args) ->
      let f_temp = gensym "f" in
      let converted_f = convert f in
      (match converted_f with
       | ANFImm (ImmVar fname) -> convert (LLApp (LLVar fname, args))
       | _ -> ANFLet (f_temp, converted_f, convert (LLApp (LLVar f_temp, args))))
    | LLFunDef (name, params, body) ->
      (* Add the function definition to our functions list *)
      functions := (name, params, convert body) :: !functions;
      (* Return a reference to the function *)
      ANFImm (ImmVar name)
  in
  let normalized = normalize_term ll_expr in
  let main_expr = convert normalized in
  !functions, main_expr
;;

let anf_of_ll functions main =
  let lambda_fns, main_anf = convert_to_anf main in
  let convert_function = function
    | LLFunDef (name, args, body) ->
      let body_fns, body_anf = convert_to_anf body in
      (* Convert the tuples to anf_function records *)
      let body_functions =
        List.map (fun (name, args, body) -> { name; args; body }) body_fns
      in
      body_functions, { name; args; body = body_anf }
    | _ -> failwith "Expected function definition"
  in
  let function_results = List.map convert_function functions in
  let lambda_functions =
    List.map (fun (name, args, body) -> { name; args; body }) lambda_fns
  in
  let all_fns =
    lambda_functions
    @ List.concat (List.map fst function_results)
    @ List.map snd function_results
  in
  { functions = all_fns; main = main_anf }
;;

(* wasm.ml *)
let wasm_type t =
  match t with
  | WInt64 -> "i64"
  | WInt32 -> "i32"
;;

let compile_to_wasm (anf_prog : anf_program) =
  let compile_function { name; args; body } =
    let locals = ref (List.length args) in
    let var_map = Hashtbl.create 32 in
    List.iteri (fun i (arg, _) -> Hashtbl.add var_map arg i) args;
    let get_or_create_local var =
      try Hashtbl.find var_map var with
      | Not_found ->
        let idx = !locals in
        incr locals;
        Hashtbl.add var_map var idx;
        idx
    in
    let rec compile_expr = function
      | ANFImm imm -> compile_imm imm
      | ANFPrim (Add, e1, e2) -> compile_imm e1 @ compile_imm e2 @ [ WAdd ]
      | ANFPrim (Sub, e1, e2) -> compile_imm e1 @ compile_imm e2 @ [ WSub ]
      | ANFPrim (Mul, e1, e2) -> compile_imm e1 @ compile_imm e2 @ [ WMul ]
      | ANFLet (x, e1, e2) ->
        let local = get_or_create_local x in
        compile_expr e1 @ [ WSet_local local ] @ compile_expr e2
      | ANFApp (f, args) -> List.concat (List.map compile_imm args) @ [ WCall f ]
    and compile_imm = function
      | ImmConst (CInt n) -> [ WConst (n, WInt64) ]
      | ImmConst (CBool true) -> [ WConst (1, WInt32) ]
      | ImmConst (CBool false) -> [ WConst (0, WInt32) ]
      | ImmConst CUnit -> [ WConst (0, WInt32) ]
      | ImmVar x -> [ WGet_local (get_or_create_local x) ]
    in
    let instrs = compile_expr body @ [ WReturn ] in
    { name; params = args; locals = !locals - List.length args; body = instrs }
  in
  let functions = List.map compile_function anf_prog.functions in
  let main = compile_function { name = "main"; args = []; body = anf_prog.main } in
  { functions = functions @ [ main ] }
;;

(* wat_printer.ml *)
let print_wat wasm_mod =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "(module\n";
  List.iter
    (fun f ->
      Buffer.add_string buffer (Printf.sprintf "  (func $%s" f.name);
      List.iter
        (fun (_, t) ->
          let typ =
            match t with
            | TInt -> "i64"
            | TBool | TUnit -> "i32"
            | _ -> failwith "Unsupported type: " ^ show_typ t
          in
          Buffer.add_string buffer (" (param " ^ typ ^ ")"))
        f.params;
      Buffer.add_string buffer " (result i32)\n";
      for _ = 1 to f.locals do
        Buffer.add_string buffer "    (local i32)\n"
      done;
      List.iter
        (function
          | WConst (n, t) ->
            (match t with
             | WInt32 ->
               Buffer.add_string buffer (Printf.sprintf "    (i32.const %d)\n" n)
             | WInt64 ->
               Buffer.add_string buffer (Printf.sprintf "    (i64.const %d)\n" n))
          | WGet_local i ->
            Buffer.add_string buffer (Printf.sprintf "    (local.get %d)\n" i)
          | WSet_local i ->
            Buffer.add_string buffer (Printf.sprintf "    (local.set %d)\n" i)
          | WCall f -> Buffer.add_string buffer (Printf.sprintf "    (call $%s)\n" f)
          | WAdd -> Buffer.add_string buffer "    (i32.add)\n"
          | WMul -> Buffer.add_string buffer "    (i32.mul)\n"
          | WSub -> Buffer.add_string buffer "    (i32.sub)\n"
          | WReturn -> Buffer.add_string buffer "    return\n")
        f.body;
      Buffer.add_string buffer "  )\n")
    wasm_mod.functions;
  Buffer.add_string buffer ")\n";
  Buffer.contents buffer
;;

(* main.ml *)
let example =
  TLet
    ( "y"
    , TInt
    , TConst (CInt 5, TInt)
    , TLet
        ( "x"
        , TArrow (TInt, TArrow (TInt, TInt))
        , TLam
            ( [ "x", TInt; "z", TInt ]
            , TPrim
                ( Add
                , TPrim (Add, TVar ("x", TInt), TVar ("y", TInt), TInt)
                , TVar ("z", TInt)
                , TInt )
            , TArrow (TInt, TArrow (TInt, TInt)) )
        , TApp
            ( TVar ("x", TArrow (TInt, TArrow (TInt, TInt)))
            , [ TConst (CInt 5, TInt); TConst (CInt 7, TInt) ]
            , TInt ) ) )
;;

let compile expr =
  let funs, main = lift_lambdas expr in
  let anf = anf_of_ll funs main in
  (* print_endline (show_anf_program anf);*)
  let wasm = compile_to_wasm anf in
  print_wat wasm
;;

let () =
  let wat = compile example in
  print_endline wat
;;
