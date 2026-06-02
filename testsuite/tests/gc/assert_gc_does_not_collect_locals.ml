(*
   test
  (targets amd64)
  (run (stdout "test passed"))
*)

let rec print_int_list ls =
  match ls with
  | [] -> ()
  | x :: xs -> 
    let () = print x in
    print_int_list xs


let failfast () =
  let () = printf "test failed" in
  exit 1

let main = 
  let local = [ 1; 2; 3 ] in
   let () = if local = [ 1; 2; 3 ] then () else failfast () in
  let () = gc_compact () in
  let () = if local = [ 1; 2; 3 ] then printf "test passed" else failfast () in
  0
