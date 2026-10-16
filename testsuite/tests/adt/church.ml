(*
test
  (targets amd64 rv32 rv64)
  (run (stdout "abs f . abs x . f (f (f (f (f x))))"))
*)

type church =
  | Zero
  | Succ of church

let pp_church oc n =
  let rec helper oc n =
    match n with
    | Zero -> fprintf oc "x"
    | Succ Zero -> fprintf oc "f x"
    | Succ (n) -> fprintf oc "f (%a)" helper n
  in
  fprintf oc "abs f . abs x . %a" helper n

let main =
  let five = Succ (Succ (Succ (Succ (Succ Zero)))) in
  let () = pp_church stdout five in
  0
