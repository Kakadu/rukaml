(*
   test
  (targets amd64)
  (run (stdout "h+e+l+l+o+ +w+o+r+l+d"))
*)

let print_chars ic sep =
  match end_of_input ic with
  | true -> ()
  | false ->
    let () = printf "%c" (input_char ic) in
    let rec loop t =
      match end_of_input ic with
      | true -> ()
      | false ->
        let () = printf "%s%c" sep (input_char ic) in
        loop 0
    in
    loop 0
;;

let main =
  let oc = open_out "foo.txt" in
  let () = fprintf oc "hello world" in
  let () = close_out oc in
  let ic = open_in "foo.txt" in
  let () = print_chars ic "+" in
  let () = close_in ic in
  0
;;
