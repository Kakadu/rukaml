(*
test
  (targets rv64 amd64)
  (flags () (--cps) (--cps --caa))
  (run (stdout "rukaml_print_int 42"))
*)

let main = (fun x -> (fun y -> let z = print y in 0) x) 42
