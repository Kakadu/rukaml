(*
test
  (targets rv64 amd64)
  (flags () (-cps) (-cps -call_arity))
  (run (stdout "rukaml_print_int 120"))
*)

let rec fack n k = if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))

let id u = u

let main =
    let rez = fack 5 id in
    let t = print rez in
    0
