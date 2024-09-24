(* pen ComRaTTlib *)

let prog = "prog.wat"

let mock_program =
  {|
(module
  (func $init (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add
  )
  (export "add" (func $add))
)
  |}
;;

let () =
  let oc = open_out prog in
  Printf.fprintf oc "%s" mock_program;
  close_out oc;
  let args = Sys.argv |> Array.to_list |> String.concat " " in
  Sys.command (Printf.sprintf "wasmer %s --invoke init %s" prog args) |> ignore
;;
