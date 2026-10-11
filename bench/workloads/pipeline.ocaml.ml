(* OCaml twin of pipeline.ml, compiled by ocamlopt/ocamlc from
   bench/harness.c. Same computation and same checksum (passed to [exit]);
   OCaml turns both tail calls into loops, so no stack-limit trick is needed
   here. *)

let rec inner n acc = if n = 0 then acc else inner (n - 1) (acc + 5)

let rec outer m acc = if m = 0 then acc else outer (m - 1) (inner 1000 acc)

let () = exit (outer 1000 0 land 255)
