(* test (targets rv64 amd64) (run) *)

let revapply x k = k x
let main =
  let z k = 0 in
  revapply 1 (fun x -> z (fun x -> 0))
