(*
test
  (targets rv64 amd64)
  (run (stdout "true" "false"))
*)

let even a k =
  if a = 0 then true
  else if a = 1 then false
  else k (a - 1)

let odd a k =
  if a = 1 then true
  else if a = 0 then false
  else k (a - 1)

let rec even1 a = even a (fun n -> odd n even1)
let main =
  let() = printf "%b\n" (even1 100) in
  let() = printf "%b\n" (even1 101) in
  0