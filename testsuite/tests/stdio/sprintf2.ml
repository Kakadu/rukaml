(*
   test
  (targets amd64)
  (run (stdout "(123+45+6)"))
*)

let rec list_concat sep strs =
  match strs with
  | [] -> ""
  | [ x ] -> sprintf "%s" x
  | x :: xs -> sprintf "%s%s%s" x sep (list_concat sep xs)
;;

let main =
  let ls = [ "123"; "45"; "6" ] in
  let t = printf "(%s)" (list_concat "+" ls) in
  0
;;
