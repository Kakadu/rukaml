(*
   test
  (targets amd64)
  (run
    (stdout "test passed"))
*)

let ls1 = [ '1'; '2'; '3' ]
let ls2 = [ '1'; '2'; '3' ]
let main = printf (if ls1 = ls2 then "test passed" else "test failed")
