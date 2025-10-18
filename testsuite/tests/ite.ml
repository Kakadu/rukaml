(*
test
  (targets rv64 amd64)
  (run
    (stdout
      "rukaml_print_int 2"
      "rukaml_print_int 1"))
*)

let foo n =
  if n = 4
  then 1
  else 2

let main =
  let x = foo 5 in
  let t = print x in
  let t = print (foo 4) in
  0
