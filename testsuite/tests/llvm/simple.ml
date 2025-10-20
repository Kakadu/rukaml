(*
test
  (targets llvm)
  (run (exit 5))
*)

let id x = x
let foo f x = x
let main = foo id 5
