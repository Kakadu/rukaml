(*
   test
  (targets amd64 rv64)
  (run (stdout "test passed"))
*)

let test t =
  match [ [ 0; 1 ]; [ 2 ] ] with
  | [] -> printf "test failed 1"
  | [ _ ] -> printf "test failed 2"
  | [] :: _ -> printf "test failed 3"
  | [ _ ] :: _ -> printf "test failed 4"
  | [ _; _ ] :: [] :: _ -> printf "test failed 5"
  | [ [ _; _ ]; [ _ ] ] -> printf "test passed"
  | _ -> printf "test failed 6"
;;

let main =
  let t = test () in
  0
;;
