(*
   test
  (targets amd64)
  (run
    (stdout
      "(1) test passed"
      "(2) test passed"
      "(3) test passed"))
*)

let test b n = printf (if b then "(%d) test passed\n" else "(%d) test failed\n") n
let not b = if b then false else true
let array1 = [| 0; 1; 2; 3; 4; 5 |]
let array2 = [| 0; 1; 2; 3; 4; 5 |]
let test1 = test (array1 = array2) 1
let () = array_set array1 1 42
let test2 = test (not (array1 = array2)) 2
let () = array_set array1 1 1
let test3 = test (array1 = array2) 3
let main = 0
