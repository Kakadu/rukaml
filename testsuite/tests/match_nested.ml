(*
test
  (targets (rv64 promote))
  (run (stdout "rukaml_print_int 3"))
*)

type 'a option =
  | Some of 'a
  | None

let main =
  let s0 = Some 0 in
  let z0 = trace_rukaml_val s0 in
  let s1 = [ None; s0 ] in
  let z1 = trace_rukaml_val s1  in
  let s2 = [ Some s1 ] in
  let z2 = trace_rukaml_val s2 in
  match s2 with
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
