(*
   test
  (targets amd64)
  (run (stdout
    "rukaml_print_int 1"
    "rukaml_print_int 2"
    "rukaml_print_int 3"
    "rukaml_print_int 4"
    "rukaml_print_int 5"
    "rukaml_print_int 6"))
*)

let () = print 1 (* prints 1 *)
let () =
  let print x = print (x + 1) in
  print 1 (* prints 2 = 1 + 1 *)
let () = print 3 (* prints 3 *)
let print x = print (x + 2)
let () = print 2 (* prints 4 = 2 + 2 *)
let () =
  let print x = print (x + 3) in
  print 0 (* prints 5 = 0 + 3 + 2 *)
let () = print 4 (* prints 6 = 4 +2 *)

let main = 0
