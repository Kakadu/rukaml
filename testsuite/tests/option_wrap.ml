(*
test
  (targets amd64)
  (run
    (stdout "rukaml_print_int 1"
            "rukaml_print_int 2"
            "rukaml_print_int 3"
            "rukaml_print_int 4"
            "rukaml_print_int 5"))
*)

type 'a option =
  | Some of 'a
  | None

let rec wrap ls =
  match ls with
  | [] -> []
  | x :: xs -> Some x :: wrap xs

let unwrap ls =
  let rec rev ls acc =
    match ls with
    | [] -> acc
    | x :: xs -> rev xs (x :: acc)
  in
  let rec aux ls acc =
    match ls with
    | [] -> Some (rev acc [])
    | Some x :: xs -> 
      aux xs (x :: acc)
    | _ -> None
  in
  aux ls []

let rec print_ls ls =
  match ls with
  | [] -> 0
  | x :: xs ->
    let t = print x in
    print_ls xs

let main =
  let ls = [ 1; 2; 3; 4; 5 ] in
  match unwrap (wrap ls) with
  | None -> 1
  | Some ls -> print_ls ls
