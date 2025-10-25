(*
test
  (targets (amd64 promote))
  (run (exit 0))

*)
let main =
  let r2 = [|true; false; false; true|] in
  length r2 = 4

