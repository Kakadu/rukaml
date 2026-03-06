
(*
test
  (targets amd64)
  (run (stdout "rukaml_print_int 1"))
*)

let main =
  let t = printf "%d" 42 in 0
