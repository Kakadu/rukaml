(*
   test
  (targets amd64)
  (run
    (stdout "(1) test passed"))
*)

type t =
  | Zero
  | One of t
  | Two of t * t
  | Three of t * t * t

let one = One Zero
let two = Two (one, one)
let three = Three (two, two, two)

let test1 =
  if
    three
    = Three (Two (One Zero, One Zero), Two (One Zero, One Zero), Two (One Zero, One Zero))
  then printf "(1) test passed\n"
  else printf "(1) test failed\n"
;;

let main = 0
