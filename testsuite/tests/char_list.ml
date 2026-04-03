(*
   test
  (targets amd64)
  (run (stdout "1234567890abcde"))
*)

(* [begin skip] *)

let printf = Stdlib.Printf.printf
let string_nth s n = s.[n]
let string_len s = Stdlib.String.length s
let string_of_char_list chs = Stdlib.String.of_seq (Stdlib.List.to_seq chs)

(* [end skip] *)

let string_to_char_list s =
  let len = string_len s in
  let rec helper n = if n >= len then [] else string_nth s n :: helper (n + 1) in
  helper 0
;;

let test1 = printf "%s" (string_of_char_list [ '1'; '2'; '3'; '4'; '5' ])
let test2 = printf "%s" (string_of_char_list (string_to_char_list "67890"))

let test3 =
  printf
    "%s"
    (string_of_char_list
       (string_to_char_list
          (string_of_char_list
             (string_to_char_list (string_of_char_list (string_to_char_list "abcde"))))))
;;

let main = 0
