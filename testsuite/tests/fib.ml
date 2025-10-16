(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 21"))
*)

let rec fib n =
  if n=0 then 0 else if n=1 then 1 else fib (n-2) + fib (n-1)

let main = let u = print (fib 8) in 0
