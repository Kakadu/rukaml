(*
test
  (targets amd64 rv32 rv64)
  (run (stdout "test passed"))
*)

type 'a option =
  | Some of 'a
  | None

let main =
  let scrut = [ Some [ None ; Some 0 ] ] in
  match scrut with
  | [] -> printf "test failed 1"
  | None :: _ ->  printf "test failed 2"
  | Some [] :: _ -> printf "test failed 3"
  | Some (None :: _) :: _ -> printf "test passed"
  | Some (Some _ :: _) :: _ ->  printf "test failed 5"
