(*
   test
  (targets amd64)
  (run (stdout "tests passed"))
*)

let string_to_char_list s =
  let len = string_len s in
  let rec helper n = if n >= len then [] else string_nth s n :: helper (n + 1) in
  helper 0
;;

let assert_equal msg a b =
  if a = b
    then ()
  else 
    let () = printf "[error] test failed: %s\n" msg
    in exit 1

let test1 () = assert_equal "test1" (string_of_char_list [ '1'; '2'; '3'; '4'; '5' ]) "12345"
let test2 () = assert_equal "test2" (string_of_char_list (string_to_char_list "67890")) "67890"

let test3 () =
  assert_equal
  "test3"
    (string_of_char_list
       (string_to_char_list
          (string_of_char_list
             (string_to_char_list (string_of_char_list (string_to_char_list "abcde")))))) "abcde"

;;

let main = 
  let () = test1 () in
  let () = test2 () in
  let () = test3 () in
  let () = printf "tests passed" in
  exit 0
