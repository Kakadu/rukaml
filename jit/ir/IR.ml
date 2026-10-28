type format =
  | TextLL (** Textual IR (.ll). *)
  | Bitcode (** Binary bitcode (.bc); not produced yet. *)

type t =
  { format : format
  ; payload : string
  ; producer : string
  }

let producer = "rukaml; llvm-ocaml-bindings 19.1.7"
let make format payload = { format; payload; producer }

let dump_to_file file t =
  let out = open_out file in
  Printf.fprintf out "%s\n" t.payload;
  close_out out
;;

let format (t : t) = t.format
let payload (t : t) = t.payload
