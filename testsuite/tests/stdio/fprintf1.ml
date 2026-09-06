(*
test
  (targets amd64 rv32 rv64)
  (run (stdout "hello world!"))
*)

let main =
  let t = printf "hello world!\n" in 0
