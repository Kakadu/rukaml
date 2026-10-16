(*
test
  (targets rv32 amd64)
  (run (stdout "rukaml_print_int 1"
                "rukaml_print_int 4"
                "rukaml_print_int 9"
                "rukaml_print_int 16"
                "rukaml_print_int 25"))
*)

let rec map f ls =
  match ls with
  | [] -> []
  | x :: xs -> f x :: map f xs

let rec print_ls ls =
  match ls with
  | [] -> 0
  | x :: xs ->
    let t = print x in
    print_ls xs

let pow2 x = x * x

let main = print_ls (map pow2 [ 1; 2; 3; 4; 5 ])
