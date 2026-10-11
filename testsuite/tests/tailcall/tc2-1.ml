(*
test
  (targets rv64 amd64)
  (run (stdout "rukaml_print_int 81"))
*)

let square x = x * x
let square_of_sum x y = square (x + y) 

let main = 
  let u = print (square_of_sum 4 5) in 0
