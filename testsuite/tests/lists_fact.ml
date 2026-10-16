(*
test
  (targets rv32 amd64 (anf promote) )
  (run (stdout "rukaml_print_int 5040"))
*)

let rec make_naturals n =
  if n = 1 then [ 1 ] else n :: make_naturals (n - 1)

let rec fold f acc ls =
  match ls with
  | [] -> acc
  | x :: xs ->
    fold f (f acc x) xs

let mul x y = x * y

let fact n = fold mul 1 (make_naturals n)

let main = print (fact 7)
