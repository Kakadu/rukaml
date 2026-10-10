(* OCaml twin of curried.ml. *)

let add a b = a + b
let rec loop n acc = if n = 0 then acc else let f = add 1 in loop (n - 1) (f acc)

let () = exit (loop 1000000 0 land 255)
