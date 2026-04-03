(*
   test
  (targets amd64)
  (run
    (stdout
      "(1) test passed"
      "(2) test passed"
      "(3) test passed"
      "(4) test passed"
      "(5) test passed"))
*)

let test b n = printf (if b then "(%d) test passed\n" else "(%d) test failed\n") n
let not b = if b then false else true

type foo =
  | Foo of int
  | Bar

type qwe =
  | Asd of foo * string
  | Zxc of foo

let x = Asd (Foo 123, "123")
let y = Asd (Foo 123, "123")
let z = Asd (Foo 234, "123")
let q = Asd (Foo 123, "234")
let w = Zxc (Foo 123)
let e = Zxc (Foo 234)
let test1 = test (x = y) 1
let test2 = test (not (x = z)) 2
let test3 = test (not (x = q)) 3
let test4 = test (not (x = w)) 4
let test5 = test (not (w = e)) 5
let main = 0
