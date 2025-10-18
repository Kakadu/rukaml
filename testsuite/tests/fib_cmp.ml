(*
test
  (targets rv64)
  (run (stdout "rukaml_print_int 21"))
*)

let rec fib n =
  let temp = n < 2 in
  if temp
  then n
  else (
    let p1 = fib (n - 1) in
    let p2 = fib (n - 2) in
    p1 + p2)

let main =
  let f = fib 8 in
  let g = print f in
  0
