(*
   test
  (targets amd64)
  (run (stdout "123456789012"))
*)

let () = printf "123"
let _ = printf "456"

let () =
  let _ = printf "789" in
  ()
;;

let _ =
  let () = printf "012" in
  0
;;

let main = 0
