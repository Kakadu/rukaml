(*
   test
  (targets amd64 rv64)
  (run (stdout "test passed"))
*)

let test () =
  match (), 1, true, "one" with
  | _, 0, _, _ -> printf "test failed 1"
  | _, _, false, _ -> printf "test failed 2"
  | _, _, _, "two" -> printf "test failed 3"
  | (), 1, true, "one" -> printf "test passed"
  | (), 1, true, _ -> printf "test failed 4"
  | (), 1, _, _ -> printf "test failed 5"
  | (), _, _, _ -> printf "test failed 6"
  | _, _, _, _ -> printf "test failed 7"
  | _ -> printf "test failed 8"
;;

let main =
  let t = test () in
  0
;;
