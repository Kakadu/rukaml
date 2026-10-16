(*
   test
  (targets amd64)
  (run (stdout "[one two three one two three one two three]"))
*)

let one = "one"
let two = "two"
let three = "three"
let one_two_three = one, two, three
let one_two_three'one_two_three'one_two_three = one_two_three, one_two_three, one_two_three

let main =
  match one_two_three'one_two_three'one_two_three with
  | (x1, x2, x3), (y1, y2, y3), (z1, z2, z3) ->
    printf "[%s %s %s %s %s %s %s %s %s]" x1 x2 x3 y1 y2 y3 z1 z2 z3
;;
