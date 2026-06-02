(*
test
  (targets amd64)
  (run (stdout "(1, true, one)"))
*)

let first (a, b, c) = a

let second (a, b, c) = b

let third (a, b, c) = c

let main =
  let tuple = (1, true, "one") in
  let t =
    printf "(%d, %b, %s)" (first tuple) (second tuple) (third tuple)
  in
  0
