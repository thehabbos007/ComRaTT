open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Comp

let name = "prog.wat"

let example = ALet ("x", TInt, ALam([("y", TInt)], APrim(Add, AVar ("y", TInt), ACstI (1, TInt), TInt)), AApp (AVar ("x", TInt), ACstI (42, TInt), TInt));;

(* woops, we cannot apply with multiple arguments.. 
  YES! nested applications, but we do not have nested lambdas?

  let add = fun x -> fun y -> x+y in add 41 1
  -> we do not support this.

*)
let _example2 = ALet ("add", TInt, ALam([("y", TInt); ("z", TInt)], APrim(Add, AVar ("y", TInt), AVar ("z", TInt), TInt)), AApp (AVar ("x", TInt), ACstI (42, TInt), TInt));;
(*
let example2better = ALet ("add", TInt, ALam([("x", TInt); ("y", TInt)], APrim(Add, AVar ("x", TInt), AVar ("y", TInt), TInt)), AApp (AVar ("add", TInt), AApp (AVar ("y", TInt), ACstI (42), TInt), TInt));;
*)
let program = init_wat example

let () =
  let oc = open_out name in
  Printf.fprintf oc "%s" program;
  close_out oc;
  let args = Sys.argv |> Array.to_list |> List.tl |> String.concat " " in
  Sys.command (Printf.sprintf "wasmer %s --invoke x %s" name args) |> ignore
;;
