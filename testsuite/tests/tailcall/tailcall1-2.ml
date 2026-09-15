(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 25"))
*)

let multiply x y = x * y
let square x = multiply x x

let main = 
  let u = print (square 5) in 0
