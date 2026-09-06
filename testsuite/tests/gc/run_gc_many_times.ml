(*
   test
  (targets amd64)
  (run (stdout "test passed"))
*)


let rec make_descending n = if n < 1 then [] else n :: make_descending (n - 1) (* [ n; n - 1; n - 2; ...; 1 ] *)

let rec assert_descending_from_n msg n ls =
  match ls with
  | [] -> ()
  | hd :: tl -> if hd = n then assert_descending_from_n msg (n - 1) tl else (let () = printf "[error] assertion failed: %s\n" msg in exit 1)


let () = assert_descending_from_n "test assert_descending_from_n" 10 (make_descending 10)

let global = make_descending 5

let rec loop n =
  if n < 1 then () else
    let local = make_descending n in
    let () = gc_compact () in
    let () = assert_descending_from_n "loop local before rec call" n local in
    let () = assert_descending_from_n "loop global before rec call" 5 global in
    let () = loop (n - 1) in
    let () = assert_descending_from_n "loop local after rec call)" n local in
    let () = assert_descending_from_n "loop global after rec call" 5 global in
    ()

  ;;

let main =
  let local = make_descending 10 in
  let () = loop 100 in
  let () = assert_descending_from_n "main local" 10 local in
  let () = assert_descending_from_n "main global" 5 global in
  let () = printf "test passed" in
  0
