(*
   test
  (targets amd64 rv64)
  (run
    (stdout "(1) test passed" "(2) test passed"))
*)

type 'a option =
  | Some of 'a
  | None

let x = Some (Some 42)
let y = Some (Some 42)
let z = Some (Some 52)
let test1 = if x = y then printf "(1) test passed\n" else printf "(1) test failed\n"
let test2 = if x = z then printf "(2) test failed\n" else printf "(2) test passed\n"
let main = 0
