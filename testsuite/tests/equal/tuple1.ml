(*
   test
  (targets amd64 rv64)
  (run
    (stdout
      "(1) test passed"
      "(2) test passed"))
*)

let test b n = printf (if b then "(%d) test passed\n" else "(%d) test failed\n") n
let not b = if b then false else true
let test1 = test ((1, 2, 3) = (0 + 1, 1 + 1, 3 * 1)) 1
let test2 = test ((true, [ 1 ]) = ((if true then true else false), 1 :: [])) 2
let main = 0
