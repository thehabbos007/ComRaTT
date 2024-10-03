open ComRaTTlib.Source
open ComRaTTlib.Annotate
open ComRaTTlib.Comp

let _let_example = ALet ("inc", TInt, ALam ([("x", TInt)], APrim(Add, AVar ("x", TInt), ACstI (1, TInt), TInt)), AApp (AVar ("inc", TInt), [ACstI (41, TInt)], TInt));;

(*
 let x = 41 in x+1
 svarer til
 ALet ("x", TInt, ACstI (42, TInt), ALam ([("x", TInt)], APrim(Add, AVar ("x", TInt), ACstI (1, TInt), TInt)))


 men vent, alle lambdaer er i en top level let binding, hvilket vil sige at den har et navn.
  eksempel:

  let x = fun y -> y+1 in x 41
  hvilket vel svarer til

  ALet ("x", TInt, ALam([("y", TInt)], APrim(Add, AVar ("y", TInt), ACstI (1, TInt), TInt))
 

*)

let _reallet = ALet ("x", TInt, ALam([("y", TInt)], APrim(Add, AVar ("y", TInt), ACstI (1, TInt), TInt)), AApp (AVar ("x", TInt), [ACstI (42, TInt)], TInt));;

let _newlet = ALet ("x", TInt, ACstI (42, TInt), ALam ([("x", TInt)], APrim(Add, AVar ("x", TInt), ACstI (1, TInt), TInt)));;

let _var_example = AVar ("x", TInt)

let _prim_example = APrim (Add, AVar ("x", TInt), ACstI (41, TInt), TInt);;

let _simple_let = ALet ("testfun", TInt, ACstI (42, TInt), ACstI (20, TInt))

let () = 
  print_endline (init_wat _reallet) |> ignore
  (*
  let _ =  print_endline (init_wat var_example) |> ignore in
  let _ = print_endline (comp prim_example) |> ignore in
  print_endline (init_wat let_example) |> ignore
  *)
  (*
  let lambda = ALam ([("x", TInt); ("y", TInt)], APrim(Add, AVar ("x", TInt), AVar ("y", TInt), TInt)) in
  print_endline (init_wat lambda) |> ignore
  *)
(*
  let add = APrim (Add, ACstI (42, TInt), ACstI (42, TInt), TInt) in
  print_endline (init_wat add) |> ignore
*)