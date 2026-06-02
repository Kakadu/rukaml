(*
   test
  (targets amd64)
  (run (stdout "test passed"))
*)

let assert_equal msg a b = if a = b then () else 
  let () = printf "[error] assersion failed: %s\n" msg in
  exit 1


let level5 () =
  let local = [ 'd', 'e', 'f' ] in
   let () = gc_compact () in
   assert_equal "level5" local  [ 'd', 'e', 'f' ]


let level4 () =
  let local = [ 'a'; 'b'; 'c' ] in
  let () = level5 () in
  assert_equal "level4" local [ 'a'; 'b'; 'c' ]


let level3 () =
  let local = [ 7; 8; 9 ] in
  let () = level4 () in
  assert_equal "level3" local [ 7; 8; 9 ]


let level2 () =
  let local = [ 4; 5; 6 ] in
  let () = level3 () in
  assert_equal "level2" local [ 4; 5; 6 ]


let level1 () =
  let local = [ 1; 2; 3 ] in
  let () = level2 () in
  assert_equal "level1" local [ 1; 2; 3 ]

  
let main = 
  let () = level1 () in
  let () = printf "test passed" in
  exit 0
