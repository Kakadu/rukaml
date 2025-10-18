(*
test
  (targets (rv64 promote))
  (flags () (-cps) (-cps -call_arity))
  (run
    (stdout
      "rukaml_print_int 1"
      "rukaml_print_int 1"))
*)

let f x =
  let y = print x in
  fun x -> x + 0

let main =
  let x = f 1 in
  let y = f 1 in
  0
