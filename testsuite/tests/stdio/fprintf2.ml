(*
   test
  (targets amd64 rv64)
  (run (stdout "true 1 a foo"))
*)

let main =
  let t = printf "%b %d %c %s\n" true 1 'a' "foo" in
  0
;;
