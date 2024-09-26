type symbol = Sym of string

let fresh_name_generator () =
  let taken = ref [] in
  let rec find hint i =
    let cand = if i = 0 then hint else hint ^ string_of_int i in
    if List.mem cand !taken
    then find hint (i + 1)
    else (
      taken := cand :: !taken;
      cand)
  in
  fun hint -> find hint 0
;;

type t =
  { fresh_symbol : string -> symbol
  ; fresh_symbol_named : string -> symbol
  ; identifiers : (symbol, string) Hashtbl.t
  ; symbol_env : (string, symbol) Hashtbl.t
  }

let make () : t =
  let identifiers = Hashtbl.create ~random:false 128 in
  let symbol_env = Hashtbl.create ~random:false 128 in
  let fresh_name = fresh_name_generator () in
  let fresh_symbol hint = Sym (fresh_name hint) in
  let fresh_symbol_named hint =
    let sym = fresh_symbol hint in
    Hashtbl.add identifiers sym hint;
    sym
  in
  { fresh_symbol; fresh_symbol_named; identifiers; symbol_env }
;;

let enter_scope { symbol_env; _ } name sym = Hashtbl.add symbol_env name sym
let exit_scope { symbol_env; _ } name = Hashtbl.remove symbol_env name

let scoped_name { symbol_env; _ } hint =
  match Hashtbl.find_opt symbol_env hint with
  | Some s -> s
  | None -> failwith (hint ^ " not found in scope")
;;

let string_of { identifiers; _ } s =
  match Hashtbl.find_opt identifiers s with
  | Some _ | None ->
    let (Sym s) = s in
    s
;;
