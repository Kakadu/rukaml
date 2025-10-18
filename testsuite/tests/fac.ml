(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 120"))
*)
let rec fac n =
  if (n=1)
  then 1
  else (
    let p = n - 1 in
    let p2 = fac p in
    n * p2)

let main =
  let f = fac 5 in
  let g = print f in
  0
