(* OCaml twin of plain.ml, compiled by ocamlopt/ocamlc from bench/harness.c.
   Same computation and same checksum (passed to [exit]); OCaml turns the tail
   call into a loop, so no stack-limit trick is needed here. *)

let rec loop n acc = if n = 0 then acc else loop (n - 1) (acc + n)

let () = exit (loop 1000000 0 land 255)
