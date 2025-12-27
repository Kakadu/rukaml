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
  | x :: xs ->
    match x with
    | None -> print 1
    | Some y ->
      match y with
      | [] -> print 2
      | z :: zs ->
        match z with
        | None -> print 3
        | Some _ -> print 4
