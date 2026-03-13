(*
test
  (targets amd64)
  (run (stdout "hello world!"))
*)

let main =
  let t = printf "hello world!" in 0
