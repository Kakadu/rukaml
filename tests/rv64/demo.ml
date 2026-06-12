
let pp_int oc n = fprintf oc "%d" n

let main =
  let t = printf "%a" pp_int 1 in
  0
;;