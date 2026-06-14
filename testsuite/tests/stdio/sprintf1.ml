(*
   test
  (targets amd64 rv64)
  (run (stdout "hello world!"))
*)

let main =
  let msg = sprintf "%s %s!" "hello" "world" in
  let () = printf "%s" msg in
  0
;;
