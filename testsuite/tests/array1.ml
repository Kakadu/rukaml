(*
test
  (targets (amd64 promote))
  (run (exit 3))

*)
let main =
  let r = [|1; 4; 3; 3|] in
  let unit = set r 1 3 in
  get r 1
