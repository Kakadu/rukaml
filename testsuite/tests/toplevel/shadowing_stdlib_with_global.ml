(*
   test
  (targets amd64)
  (run (exit 0))
*)

let exit _ = ()

let main = 
  let () = exit 1 in 0
