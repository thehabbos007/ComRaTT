open Source
open Annotate

type immexpr =
  | ImmConst of const
  | ImmVar of string
[@@deriving show, eq]

type anfexpr =
  | ANFImm of immexpr
  | ANFPrim of binop * immexpr * immexpr
  | ANFLet of string * anfexpr * anfexpr
  | ANFApp of string * immexpr list
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
