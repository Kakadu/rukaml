(*
   test
  (targets amd64)
  (run (stdout "you should see this message"))
*)


let () = printf "you should see this message"

let main = 0

let () = printf "test failed"
