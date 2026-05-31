(*
test
  (targets (rv64 promote))
  (run (stdout
  "BLOCK: 0x152a8, tag=0, size=1"
  " 0 -> Int 0"
  "BLOCK: 0x15468, tag=1, size=2"
  " BLOCK: 0x15328, tag=1, size=0"
  " BLOCK: 0x153a8, tag=1, size=2"
  "  BLOCK: 0x152a8, tag=0, size=1"
  "   0 -> Int 0"
  "  BLOCK: 0x15368, tag=0, size=0"
  "BLOCK: 0x155e8, tag=1, size=2"
  " BLOCK: 0x15528, tag=0, size=1"
  "  BLOCK: 0x15468, tag=1, size=2"
  "   BLOCK: 0x15328, tag=1, size=0"
  "   BLOCK: 0x153a8, tag=1, size=2"
  "    BLOCK: 0x152a8, tag=0, size=1"
  "     0 -> Int 0"
  "    BLOCK: 0x15368, tag=0, size=0"
  " BLOCK: 0x155a8, tag=0, size=0"
  "rukaml_print_int 3"
  ))
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
