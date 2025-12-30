(*
test
  (targets anf amd64 (rv64 promote))
  (run (stdout "rukaml_print_int 0"
               "rukaml_print_int 1"
               "rukaml_print_int 0"
               "rukaml_print_int 0"))
*)

type 'a option =
  | Some of 'a
  | None

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



let main =
   1
