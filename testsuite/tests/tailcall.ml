(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 42"))
*)
let rec foo x = 
  if x = 0
  then 42
  else foo (x - 1)

let main = let u = print (foo 100000000) in 0
