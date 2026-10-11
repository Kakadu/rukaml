(*
test
  (targets rv64 amd64)
  (run (stdout 0))
*)

let rec fac_acc n acc = 
  if n <= 1 then acc
  else fac_acc (n - 1) (n * acc)

(* Should work with enabled and disabled optimization *)
(* To illustrate that the stack doesn't grow *)
let main =
  let() = printf "%d\n" (fac_acc 10000 1) in
  0