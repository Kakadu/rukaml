(*
test
  (targets rv64 amd64)
  (run (stdout 240 42))
*)

let rec fac_cps n k = 
  if n <= 1 then k 1
  else fac_cps (n - 1) (fun r -> k (n * r))

let multiple_by_two n = n * 2
let to42 n = 42
let main =
  let() = printf "%d\n" (fac_cps 5 multiple_by_two) in
  let() = printf "%d\n" (fac_cps 10 to42) in
  0