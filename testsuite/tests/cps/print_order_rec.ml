(*
test
  (targets rv64)
  (flags (-cps) (-cps -call_arity))
  (run
    (stdout
      "rukaml_print_int 2"
      "rukaml_print_int 3"
      "rukaml_print_int 4"))
*)

let rec f x =
  if x < 2
  then fun y -> y + 1
  else (
    let h = f (x - 1) in
    let z = print x in
    h)


let main = f 4 1
