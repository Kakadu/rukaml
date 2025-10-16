(*
test
  (targets (rv64 promote))
  (run (stdout "rukaml_print_int 1"))
*)

(* should print 20 lol *)

let prod a b c = 2 * c

let main =
  let w = prod 1 8 10 in
  let u = print w in
  0
