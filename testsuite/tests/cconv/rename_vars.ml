(*
test
  (targets amd64)
  (run (exit 0))
*)

let foo () = 1

let () =
  let foo () = 2 in
  ()

let main =
  if foo () = 1
  then exit 0
  else exit 1
