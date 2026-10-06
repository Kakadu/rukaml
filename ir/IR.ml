type format =
  | TextLL
  (** Textual IR (.ll), parsed with [Llvm_irreader] on the OCaml
                side and [parseAssemblyString] on the ORC side. *)
  | Bitcode
  (** Binary bitcode (.bc). Reserved: not produced yet, switch is
                a one-liner in [LLVM_impl.to_ir] once wanted. *)

type t =
  { format : format
  ; payload : string
  ; producer : string
  }

let producer = "rukaml; llvm-ocaml-bindings 19.1.7"

let make format payload = { format; payload; producer }

let format (t : t) = t.format
let payload (t : t) = t.payload
