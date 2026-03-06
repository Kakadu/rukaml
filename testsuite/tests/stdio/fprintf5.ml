(*
test
  (targets amd64)
  (run (stdout "rukaml_print_int 1"))
*)

let main =
  let pp_pair pp_left pp_right oc (a, b) =
    fprintf oc "%a, %a" pp_left a pp_right b
  in
  let pp_bool oc b = fprintf oc "%b" b in
  let pp_str oc s = fprintf oc "%s" s in
  let u =
    printf "(%a)" (pp_pair pp_bool pp_str) (true, "false")
  in 0
