(*
test
  (targets (rv64 promote))
  (run (stdout
  "BLOCK: 0x152a8, tag=0, size=1"
  " 0 -> Int 0"
  "BLOCK: 0x152d8, tag=1, size=2"
  " BLOCK: 0x152f0, tag=1, size=0"
  " BLOCK: 0x152b8, tag=1, size=2"
  "  BLOCK: 0x152a8, tag=0, size=1"
  "   0 -> Int 0"
  "  BLOCK: 0x152d0, tag=0, size=0"
  "BLOCK: 0x15308, tag=1, size=2"
  " BLOCK: 0x152f8, tag=0, size=1"
  "  BLOCK: 0x152d8, tag=1, size=2"
  "   BLOCK: 0x152f0, tag=1, size=0"
  "   BLOCK: 0x152b8, tag=1, size=2"
  "    BLOCK: 0x152a8, tag=0, size=1"
  "     0 -> Int 0"
  "    BLOCK: 0x152d0, tag=0, size=0"
  " BLOCK: 0x15320, tag=0, size=0"
  "rukaml_print_int 3"
  ))
*)

type 'a option =
  | Some of 'a
  | None

let main =
  let s0 = Some 0 in
  let u = trace_rukaml_val s0 in
  let s1 = [ None; s0 ] in
  let u = trace_rukaml_val s1  in
  let s2 = [ Some s1 ] in
  let u = trace_rukaml_val s2 in
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
