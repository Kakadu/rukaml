(*
   test
  (targets anf amd64)
  (run (exit 1) (stdout "[error] assersion failed: outer_local before gc in inner"))
*)

(* This test requires fixing because it is built with a concept of physical equality in mind *)

let global = (1, 2, 3)

let assert_equal msg a b = if a = b then () else
  let () = printf "[error] assersion failed: %s\n" msg in
  exit 1

let inner outer_local () =
    let inner_local = (7, 8, 9) in
    let () = assert_equal "inner_local before gc in inner" inner_local (7, 8, 9) in
    let () = assert_equal "outer_local before gc in inner" outer_local (4, 5, 6) in
    let () = gc_compact () in
    let () = assert_equal "inner_local after gc in inner" inner_local (7, 8, 9) in
    let () = assert_equal "outer_local after gc int inner" outer_local (4, 5, 6) in
    ()



let outer () =
  let outer_local = (4, 5, 6) in
  let () = assert_equal "outer_local before inner called" outer_local (4, 5, 6) in
  let () = inner outer_local () in
  let () = assert_equal "outer_local after inner called" outer_local (4, 5, 6) in
  let () = gc_compact () in
  let () = assert_equal "outer_local after gc called from outer" outer_local (4, 5, 6) in
  ()

let main =
  let () = outer () in
  let () = assert_equal "global after outer in main" global (1, 2, 3) in
  let () = printf "test passed" in
  exit 0
