(*
test
  (targets rv64 amd64)
  (flags () (-cps) (-cps -call_arity))
  (run (stdout "rukaml_print_int 24"))
*)

let rec fac n =
  if n = 1 then 1
  else
    let p = n - 1 in
    let p2 = fac p in
    n * p2

let main =
  let n = fac 4 in
  let t = print n in
  0
