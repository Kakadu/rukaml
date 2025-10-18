(*
test
  (targets rv64 amd64)
  (flags () (-cps) (-cps -call_arity))
  (run
    (stdout
      "rukaml_print_int 1000"
      "rukaml_print_int 100"
      "rukaml_print_int 1"
      "rukaml_print_int 10"
      "rukaml_print_int 100"))
*)

let add a b c =
  let wa = print a in
  let wb = print b in
  let wc = print c in
  0

let main =
  let a1 = add 1  in
  let tt0 = print 1000 in
  let a2 = a1 10  in
  let tt1 = print 100 in
  let a3 = a2 100  in
  0
