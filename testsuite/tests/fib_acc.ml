(*
test
  (targets rv64 amd64)
  (flags () (-cps) (-cps -call_arity))
  (run (stdout "rukaml_print_int 3"))
*)

let rec fib_acc a b n =
  if n=1 then b
  else
    let n1 = n-1 in
    let ab = a+b in
    fib_acc b ab n1

let main =
  let f = fib_acc 0 1 4 in
  let g = print f in
  0
