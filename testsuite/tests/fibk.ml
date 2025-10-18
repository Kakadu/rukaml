(*
test
  (targets rv64)
  (run (stdout "rukaml_print_int 8"))
*)

let id x = x

let rec fib n k =
  if n < 2 then k n
  else
    fib (n - 1) (fun p1 -> fib (n-2) (fun p2 -> k (p1+p2)))

let main =
  let w = fib 6 id in
  let z = print w in
  0
