(*
test
  (targets amd64)
  (run (stdout "rukaml_print_int 3"))
*)

type 'a option =
  | Some of 'a
  | None

let main =
  let scrut = [ Some [ None ; Some 0 ] ] in
  match scrut with
  | [] -> print 0
  | None :: _ -> print 1
  | Some [] :: _ -> print 2
  | Some (None :: _) :: _ -> print 3
  | Some (Some _ :: _) :: _ -> print 4
