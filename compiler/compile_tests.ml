open Miniml
open Format

let iter =
  {|
let iter n =
  let rec loop m =
    if m=n
    then m
    else loop (1+m)
  in
  loop 0|}
;;

let%expect_test "Free vars in expr" =
  let _, _, expr = Parsing.parse_vb_exn iter in
  Format.printf "%a\n" CConv.String_set.pp (CConv.free_vars_of_expr expr);
  [%expect "{set| +, =, |set}"]
;;

let%expect_test "Free vars in expr" =
  let _, _, expr = Parsing.parse_vb_exn "let x = let rec loop m = m in loop x" in
  Format.printf "%a\n" CConv.String_set.pp (CConv.free_vars_of_expr expr);
  [%expect "{set| x, |set}"]
;;
