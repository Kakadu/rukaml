(*
test
  (targets llvm)
  (run (exit 27))
*)

let mydiv a b = (a+b, a)
let f a b =
  let (u,v) = mydiv a b in
  u+v

let main = f 7 13
