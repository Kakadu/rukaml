(*
  test
  (targets rv64)
  (run (stdout "-8"))
*)

let wordsize = 4

type ptype =
  | Ptype_int
  | Ptype_void

let sizeof_ctype ptype =
  match ptype with
  | Ptype_int -> wordsize
  | Ptype_void -> 0
;;

let main =
  printf "%d\n" (0 - wordsize * 2)