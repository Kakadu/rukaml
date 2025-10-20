(*
test
  (targets amd64)
  (run (stdout "rukaml_print_int 10"))
*)

let main = print (if 1=1 then 10 else 20)
