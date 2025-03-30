open Frontend
open Parsing
open Angstrom

let test_stru str =
  match Angstrom.parse_string ~consume:All (Angstrom.many value_binding) str with
  | Result.Error e -> Format.eprintf "Error: %s\n" e
  | Ok xs -> List.iter (Format.printf "%a\n%!" Pprint.pp_value_binding) xs
;;

let wrap_parse_exn parser printer str =
  match parse_string ~consume:All parser str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing"
  | Ok r -> Format.printf "@[%a@]\n%!" printer r
;;

let parse_expr_exn str =
  (* Stdlib.Format.printf "parsing a string '%s'\n%!" str; *)
  match parse_pack pack.prio str with
  | Result.Error e ->
    Format.eprintf "Error: %s\n" e;
    failwith "Error during parsing"
  | Ok r -> Format.printf "@[%a@]\n%!" Pprint.pp_expr r
;;

let print_end_parse_exn input =
  Format.printf "@[%a@]\n%!" Pprint.pp_value_binding (parse_vb_exn input)
;;

let%expect_test _ =
  test_stru
    {|
    let mul5 x = repeat 5 (fun acc -> x) 0
    let rec fac n = if n = 1 then n else n * (fac (n - 1))
    let main x = 32
     |};
  [%expect
    {|
    let mul5 x = repeat 5 (fun acc -> x) 0
    let rec fac n = if n = 1 then n else n * (fac (n - 1))
    let main x = 32 |}]
;;

let%expect_test _ =
  print_end_parse_exn {|let mul5 x = repeat 5 (fun acc -> x) 0
     |};
  [%expect {|
    let mul5 x = repeat 5 (fun acc -> x) 0 |}]
;;

let%expect_test _ =
  wrap_parse_exn (pack.expr pack) Pprint.pp_expr {|5+1|};
  [%expect {| (5 + 1) |}]
;;

let%expect_test _ =
  print_end_parse_exn {|let mul5 = 5+1 |};
  [%expect {| let mul5 = 5 + 1 |}]
;;

let%expect_test _ =
  print_end_parse_exn
    {|
    let foo = if n>0 then 1 else (if n>0 then 1 else (if n>0 then 1 else (if n>0 then 1 else 2)))
     |};
  [%expect
    {|
    let foo = if n > 0 then 1 else if n > 0 then 1 else if n > 0 then 1 else
                                                                        if n > 0
                                                                        then 1
                                                                        else 2 |}]
;;

let%expect_test _ =
  print_end_parse_exn
    {|
    let foo = let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in  let x = 5 in 11
     |};
  [%expect
    {|
    let foo = let x = 5 in let x = 5 in let x = 5 in let x = 5 in let x = 5 in
                                                                  let x = 5 in
                                                                  let x = 5 in
                                                                  let x = 5 in
                                                                  11 |}]
;;

let%expect_test _ =
  print_end_parse_exn {|
    let a = reeee (fun acc -> x)
     |};
  [%expect {| let a = reeee (fun acc -> x) |}]
;;

let%expect_test _ =
  print_end_parse_exn {| let fresh_1 x acc = x |};
  print_end_parse_exn {| let mul5 x = repeat 5 (fresh_1 x) 0 |};
  [%expect {|
    let fresh_1 x acc = x
    let mul5 x = repeat 5 (fresh_1 x) 0 |}]
;;

let%expect_test _ =
  parse_expr_exn {| 1+1 |};
  [%expect {|
    (1 + 1) |}]
;;

open Format

let%expect_test _ =
  let vb1 = parse_vb_exn {| let fresh_1 x acc = x |} in
  let vb2 = parse_vb_exn {| let mul5 x = repeat 5 (fresh_1 x) 0 |} in
  printf
    "@[%a@]\n\t~~[%d value bindings]~~>\n@[<v>%a@]\n"
    Pprint.pp_value_binding
    vb1
    2
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@.") Pprint.pp_value_binding)
    [ vb1; vb2 ];
  [%expect
    {|
    let fresh_1 x acc = x
    	~~[2 value bindings]~~>
    let fresh_1 x acc = x
    let mul5 x = repeat 5 (fresh_1 x) 0
|}]
;;
