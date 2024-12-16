open Source

(** Symbol i.e. variables *)
type sym = string [@@deriving show, eq]

module Environment = Map.Make (struct
    type t = sym

    let compare = String.compare
  end)

type typ =
  | TInt
  | TBool
  | TUnit
  | TVar of typ_var
  | TArrow of typ * typ
  | TList of typ
[@@deriving show, eq]

and typ_var = (typ_var_kind * int) ref (* kind and binding level *)

and typ_var_kind =
  | Link of typ
  | Unlink of int
[@@deriving show, eq]

type typ_scheme = TypeScheme of typ_var list * typ (* type variables and type *)
[@@deriving show, eq]

type tenv = (string * typ_scheme) list [@@deriving show, eq]

type typed_expr =
  | TConst of const * typ
  | TName of sym * typ
  | TLam of (sym * typ) list * typed_expr * typ
  | TFunDef of sym * (sym * typ) list * typed_expr * typ
  | TApp of typed_expr * typed_expr list * typ
  | TPrim of binop * typed_expr * typed_expr * typ
  | TLet of sym * typ * typed_expr * typed_expr
  | TIfThenElse of
      typed_expr
      * typ
      * typed_expr
      * typed_expr
      * typ (* guard, type of guard, then-branch, else-branch and type of branches*)
[@@deriving show, eq]

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "()"
  | TVar tv -> "TVar (" ^ string_of_type_var_kind tv ^ ")"
  | TArrow (t1, t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TList ty -> "List " ^ string_of_type ty

and string_of_type_var_kind tvar =
  match fst !tvar with
  | Link t -> "Link " ^ string_of_type t
  | Unlink n -> "Unlink " ^ string_of_int n
;;

let type_of expr =
  match expr with
  | TConst (_, t)
  | TName (_, t)
  | TLam (_, _, t)
  | TFunDef (_, _, _, t)
  | TApp (_, _, t)
  | TPrim (_, _, _, t)
  | TLet (_, t, _, _) -> t
  | TIfThenElse (_, _, _, _, t) -> t
;;

let convert_params_to_arrow params ret_type =
  List.fold_right (fun (_, t) acc -> TArrow (t, acc)) params ret_type
;;

let rec pop_n_tarrow n tarrow =
  match tarrow with
  | _ when n = 0 -> tarrow
  | TArrow (_, r) when n = 1 -> r
  | TArrow (_, r) -> pop_n_tarrow (n - 1) r
  | _ ->
    failwith ("Trying to pop " ^ string_of_int n ^ " args from " ^ string_of_type tarrow)
;;

let rec union (xs, ys) =
  match xs with
  | [] -> ys
  | x :: xr -> if List.mem x ys then union (xr, ys) else x :: union (xr, ys)
;;

let rec unique xs =
  match xs with
  | [] -> []
  | x :: xr -> if List.mem x xr then unique xr else x :: unique xr
;;

let update_tv_kind tv newKind = tv := newKind, snd !tv
let update_tv_level tv newLevel = tv := fst !tv, newLevel

let rec find t =
  match t with
  | TVar tv ->
    (match !tv with
     | Link target, _ ->
       let existing = find target in
       update_tv_kind tv (Link existing);
       existing
     | _ -> t)
  | _ -> t
;;

let rec free_tvs t : typ_var list =
  match find t with
  | TBool | TInt | TUnit -> []
  | TVar tv -> [ tv ]
  | TArrow (t1, t2) -> union (free_tvs t1, free_tvs t2)
  | TList ty -> free_tvs ty
;;

let occur_check tv tyvars =
  if List.mem tv tyvars then failwith "type error: circularity" else ()
;;

let prune_level max_level tvs =
  let reduce_level tyvar =
    let _, level = !tyvar in
    update_tv_level tyvar (min level max_level)
  in
  List.iter reduce_level tvs
;;

let link_var_to_typ tv typ =
  let _, level = !tv in
  let fvs = free_tvs typ in
  occur_check tv fvs;
  prune_level level fvs;
  update_tv_kind tv (Link typ)
;;

let rec unify t1 t2 : unit =
  let t1' = find t1 in
  let t2' = find t2 in
  match t1', t2' with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TUnit, TUnit -> ()
  | TArrow (t11, t12), TArrow (t21, t22) ->
    unify t11 t21;
    unify t12 t22
  | TVar tv1, TVar tv2 ->
    let _, tv1level = !tv1 in
    let _, tv2level = !tv2 in
    if tv1 = tv2
    then ()
    else if tv1level < tv2level
    then link_var_to_typ tv1 t2'
    else link_var_to_typ tv2 t1'
  | TVar tv1, _ -> link_var_to_typ tv1 t2'
  | _, TVar tv2 -> link_var_to_typ tv2 t1'
  | TList t1, TList t2 -> unify t1 t2
  | TInt, t -> failwith ("type error: int and " ^ string_of_type t)
  | TBool, t -> failwith ("type error: bool and " ^ string_of_type t)
  | TArrow _, t -> failwith ("type error: function and " ^ string_of_type t)
  | TUnit, t -> failwith ("type error: unit and " ^ string_of_type t)
  | TList _, t -> failwith ("type error list and " ^ string_of_type t)
;;

let next_type_var = ref 0

let fresh_type_var level =
  incr next_type_var;
  ref (Unlink !next_type_var, level)
;;

let generalize level (t : typ) : typ_scheme =
  let not_free_in_context tyvar =
    let _, linkLevel = !tyvar in
    linkLevel > level
  in
  let tvs = List.filter not_free_in_context (free_tvs t) in
  TypeScheme (unique tvs, t)
;;

let rec copy_typ subst t : typ =
  match t with
  | TBool | TInt | TUnit -> t
  | TVar tv ->
    let rec loop subst1 =
      match subst1 with
      | (tyvar1, type1) :: rest -> if tyvar1 = tv then type1 else loop rest
      | [] ->
        (match !tv with
         | Unlink _, _ -> t
         | Link t1, _ -> copy_typ subst t1)
    in
    loop subst
  | TArrow (t1, t2) -> TArrow (copy_typ subst t1, copy_typ subst t2)
  | TList t -> TList (copy_typ subst t)
;;

let specialize level (TypeScheme (tvs, t)) : typ =
  let bindfresh tv = tv, TVar (fresh_type_var level) in
  match tvs with
  | [] -> t
  | _ ->
    let subst = List.map bindfresh tvs in
    copy_typ subst t
;;

let rec typ (level : int) (env : tenv) (e : expr) : typed_expr * typ =
  match e with
  | Const (CInt v) -> TConst (CInt v, TInt), TInt
  | Const (CBool v) -> TConst (CBool v, TBool), TBool
  | Const CUnit -> TConst (CUnit, TUnit), TUnit
  | Const CNil ->
    let list_typ = TList (TVar (fresh_type_var level)) in
    TConst (CNil, list_typ), list_typ
  | Var x ->
    let typ = specialize level (List.assoc x env) in
    TName (x, typ), typ
  | Prim (op, e1, e2) ->
    let e1, t1 = typ level env e1 in
    let e2, t2 = typ level env e2 in
    let typ =
      match op with
      | Mul | Add | Sub | Div ->
        unify TInt t2;
        unify t1 t2;
        TInt
      | Eq | Neq ->
        unify t1 t2;
        TBool
      | Lt | Lte | Gt | Gte ->
        unify TInt t2;
        unify t1 t2;
        TBool
      | Cons ->
        unify (TList t1) t2;
        t2
    in
    TPrim (op, e1, e2, typ), typ
  | Let (name, e1, e2) ->
    let level' = level + 1 in
    let e1, t1 = typ level' env e1 in
    let let_env = (name, generalize level t1) :: env in
    let e2, t2 = typ level let_env e2 in
    TLet (name, t2, e1, e2), t2
  | IfThenElse (ec, et, ef) ->
    let ec, tc = typ level env ec in
    let et, tt = typ level env et in
    let ef, tf = typ level env ef in
    unify TBool tc;
    unify tt tf;
    TIfThenElse (ec, tc, et, ef, tt), tt
  | FunDef (name, args, body) ->
    let level' = level + 1 in
    let fun_typ = TVar (fresh_type_var level') in
    let arg_typs = List.map (fun arg -> arg, TVar (fresh_type_var level')) args in
    let body_env =
      ((name, TypeScheme ([], fun_typ))
       :: List.map (fun (arg, arg_typ) -> arg, TypeScheme ([], arg_typ)) arg_typs)
      @ env
    in
    let body, body_typ = typ level' body_env body in
    let expanded_fun_typ = convert_params_to_arrow arg_typs body_typ in
    unify fun_typ expanded_fun_typ;
    TFunDef (name, arg_typs, body, expanded_fun_typ), expanded_fun_typ
  | App (f, arg) ->
    let f, tf = typ level env f in
    let arg, targ = typ level env arg in
    let tr = TVar (fresh_type_var level) in
    unify tf (TArrow (targ, tr));
    TApp (f, [ arg ], tr), tr
  | Lam (args, body) ->
    let level' = level + 1 in
    let arg_typs = List.map (fun arg -> arg, TVar (fresh_type_var level')) args in
    let lam_env =
      List.map (fun (arg, arg_typ) -> arg, TypeScheme ([], arg_typ)) arg_typs @ env
    in
    let body, body_typ = typ level' lam_env body in
    let lam_typ = convert_params_to_arrow arg_typs body_typ in
    TLam (arg_typs, body, lam_typ), lam_typ
  | Delay e ->
    let e, t = typ level env e in
    let ret_typ = TArrow (TUnit, t) in
    TLam ([ "#advance_unit", TUnit ], e, ret_typ), ret_typ
  | Advance name ->
    let delayed_typ = specialize level (List.assoc name env) in
    let ret_type = pop_n_tarrow 1 delayed_typ in
    TApp (TName (name, delayed_typ), [ TConst (CUnit, TUnit) ], ret_type), ret_type
;;

let reset_tv_counter () = next_type_var := 0

let get_fundef_name = function
  | FunDef (name, _, _) -> name
  | _ -> failwith "expected FunDef"
;;

let rec resolve_type t =
  match t with
  | TVar tv ->
    (match !tv with
     | Link target, _ -> target
     | _ -> t)
  | TArrow (t1, t2) -> TArrow (resolve_type t1, resolve_type t2)
  | _ -> t
;;

let rec simplify_types = function
  | TConst (c, t) -> TConst (c, resolve_type t)
  | TName (name, t) -> TName (name, resolve_type t)
  | TLam (params, body, t) ->
    TLam
      ( List.map (fun (p, t) -> p, resolve_type t) params
      , simplify_types body
      , resolve_type t )
  | TFunDef (name, params, body, t) ->
    TFunDef
      ( name
      , List.map (fun (p, t) -> p, resolve_type t) params
      , simplify_types body
      , resolve_type t )
  | TApp (f, args, t) ->
    TApp (simplify_types f, List.map simplify_types args, resolve_type t)
  | TPrim (op, e1, e2, t) ->
    TPrim (op, simplify_types e1, simplify_types e2, resolve_type t)
  | TLet (name, t, e1, e2) ->
    TLet (name, resolve_type t, simplify_types e1, simplify_types e2)
  | TIfThenElse (cond, tc, t1, t2, t) ->
    TIfThenElse
      ( simplify_types cond
      , resolve_type tc
      , simplify_types t1
      , simplify_types t2
      , resolve_type t )
;;

(* Type inference on all exprs (which are essentially only FunDefs here *)
let infer_all exprs =
  reset_tv_counter ();
  let rec aux env acc = function
    | [] -> List.rev acc
    | expr :: rest ->
      let typed_expr, t = typ 0 env expr in
      let inner = get_fundef_name expr, TypeScheme ([], t) in
      aux (inner :: env) (typed_expr :: acc) rest
  in
  let inferred = aux [] [] exprs in
  List.map simplify_types inferred
;;
