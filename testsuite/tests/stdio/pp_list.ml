(*
   test
  (targets amd64)
  (run (stdout "[ 1, 2; 3, 4; 5, 6; 7, 8 ]"))
*)

let pp_print_list pp_sep pp_item oc ls =
  let rec helper ls =
    match ls with
    | [] -> ()
    | x :: xs ->
      let t = pp_sep oc 0 in
      let t = pp_item oc x in
      helper xs
  in
  match ls with
  | [] -> ()
  | x :: xs ->
    let t = pp_item oc x in
    helper xs
;;

let main =
  let ls = [ 1, 2; 3, 4; 5, 6; 7, 8 ] in
  let pp_sep oc t = fprintf oc "; " in
  let pp_item oc (x, y) = fprintf oc "%d, %d" x y in
  printf "[ %a ]" (pp_print_list pp_sep pp_item) ls
;;
