(*
   test
  (targets amd64)
  (run (stdout "1234567"))
*)

let global = [ 1; 2; 3 ]

let f () = global

let global = [ 4; 5; 6; 7 ]

let g () = global

let rec print_int_list ls =
  match ls with
  | [] -> ()
  | x :: xs ->
    let () = printf "%d" x in
    print_int_list xs

let main =
  let () = print_int_list (f ()) in
  let () = print_int_list (g ()) in 0
