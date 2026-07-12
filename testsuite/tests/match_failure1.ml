(*
test
  (targets rv32 amd64)
  (run (exit 1) (stdout "Match failure"))
*)

type foo =
  | I of int
  | B of bool
  | U of unit

let bad_matching x =
  match x with
  | I n -> n
  | B b -> if b then 1 else 0

let main = bad_matching (U ())
