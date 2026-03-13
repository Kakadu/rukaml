(*
   test
  (targets amd64)
  (run (stdout "1 | true | a | foo"))
*)

let pp_int oc n = fprintf oc "%d" n
let pp_char oc c = fprintf oc "%c" c
let pp_bool oc b = fprintf oc "%b" b
let pp_string oc s = fprintf oc "%s" s

let main =
  let t = printf "%a | %a | %a | %a" pp_int 1 pp_bool true pp_char 'a' pp_string "foo" in
  0
;;
