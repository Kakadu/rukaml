(*
test
  (targets llvm)
  (run (exit 24))
*)

let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m -> k (n * m))
let main = fack 4 (fun x -> x)
