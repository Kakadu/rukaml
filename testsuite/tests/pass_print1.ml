(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 11"))
*)

let addk a b k = k (a+b)
let foo f x = f x

let main =
  let tmp = foo print in
  addk 1 10 tmp
