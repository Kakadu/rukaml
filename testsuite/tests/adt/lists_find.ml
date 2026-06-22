(*
test
  (targets amd64 rv64)
  (run
    (stdout "test passed"))
*)

type 'a option =
  | Some of 'a
  | None

let rec find pred ls =
  match ls with
  | [] -> None
  | x :: xs ->
    if pred x then Some x else find pred xs

let rec forall pred ls =
  match ls with
  | [] -> true
  | x :: xs ->
    if pred x then forall pred xs else false

let not x = if x then false else true

let rec is_even_positive x = if x = 0 then true else not (is_even_positive (x - 1))

let rec print_ls ls =
  match ls with
  | [] -> 0
  | x :: xs ->
    let t = print x in
    print_ls xs

let main =
  let ls1 = [ 1; 2; 3; 4; 5 ] in
  let ls2 = [ 2; 4; 6 ] in
  let ls3 = [ 1; 0; 1 ] in
  match find (forall is_even_positive) [ ls1; ls2; ls3 ] with
  | Some [ 2; 4; 6 ] -> printf "test passed"
  | _ ->
    let () = printf "test failed" in
    exit 1
