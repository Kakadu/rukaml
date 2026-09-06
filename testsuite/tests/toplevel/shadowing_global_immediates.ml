(*
   test
  (targets amd64)
  (run (stdout
    "rukaml_print_int 10"
    "rukaml_print_int 20"
  ))
*)

let x = 10

let f () = x

let x = 20

let g () = x

let main =
  let () = print ( f() ) in
  let () = print ( g() ) in 0
