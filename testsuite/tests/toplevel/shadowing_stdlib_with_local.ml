(*
   test
  (targets amd64)
  (run (exit 0))
*)

let foo () =
  let exit _ = () in
  exit 1

let main = 
  let () = foo () in
  exit 0
