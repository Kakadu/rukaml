(*
test
  (targets (anf promote) amd64 rv32 rv64)
  (run (stdout "rukaml_print_int 0"
               "rukaml_print_int 1"
               "rukaml_print_int 0"
               "rukaml_print_int 0"))
*)

type 'a option =
  | None
  | Some of 'a

let bind opt f =
  match opt with
  | None -> None
  | Some x -> f x

let return x = Some x

let int_of_bool x = if x then 1 else 0

let rec bool_of_int_opt x =
  match x with
  | 0 -> Some false
  | 1 -> Some true
  | _ -> None

let rec map_ints_to_bools lst =
  match lst with
  | [] -> Some []
  | x :: xs ->
      bind (bool_of_int_opt x) (fun b ->
      bind (map_ints_to_bools xs) (fun bs ->
        return (b :: bs)
      ))

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

let main =
  match map_ints_to_bools [ 0; 1; 0; 0 ] with
  | Some ls -> print_ls (map int_of_bool ls)
  | None -> 1
