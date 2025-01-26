open Miniml
open CConv
open Format

let%expect_test " " =
  let left = String_set.of_list [ "a"; "b"; "c" ] in
  let other = String_set.of_list [ "d"; "b"; "c" ] in
  let set = without ~other left in
  Format.printf "%a\n" String_set.pp set;
  [%expect {|{set| a, |set} |}]
;;

let%expect_test " " =
  let ast = Parsing.parse_vb_exn "let ter loop = (fun n -> loop n 0) loop" in
  Format.printf "%a\n%!" Pprint.pp_value_binding (CConv.simplify_vb ast);
  [%expect {| let ter n = loop n 0 |}]
;;

let wrap ?(verbose = false) ?(standart_globals = standart_globals) input =
  set_logging verbose;
  let ast = Parsing.parse_vb_exn input in
  let rez = conv ~standart_globals ast in
  Format.printf
    "@[%a@]\n\t~~[%d value bindings]~~>\n@[<v>%a@]\n"
    Pprint.pp_value_binding
    ast
    (List.length rez)
    (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@.") Pprint.pp_value_binding)
    rez;
  set_logging false
;;

let%expect_test " " =
  wrap Compile_tests.iter;
  [%expect
    {|
      let iter n = let rec loop m = if m = n then m else loop (1 + m) in loop 0
      	~~[2 value bindings]~~>

      let rec loop n m = if m = n then m else loop n (1 + m)
      let iter n = loop n 0 |}]
;;

let anon1 = {|
let mul5 x = repeat 5 (fun acc -> x) 0|}

let%expect_test " " =
  wrap ~verbose:false ~standart_globals:(String_set.add "repeat" standart_globals) anon1;
  [%expect
    {|
    let mul5 x = repeat 5 (fun acc -> x) 0
    	~~[2 value bindings]~~>
    let fresh_1
                                                                    x acc =
                                                                      x
    let mul5 x = repeat 5 (fresh_1 x) 0
       |}]
;;

let%expect_test "iter" =
  wrap
    ~standart_globals:(String_set.add "repeat" standart_globals)
    "let iter n = let rec loop m = n + m in loop n";
  [%expect
    {|
    let iter n = let rec loop m = n + m in loop n
    	~~[2 value bindings]~~>

    let rec loop n m = n + m
    let iter n = loop n n
       |}]
;;

let repeat =
  {|
let repeat n f =
  let rec loop m last =
    if m<n
    then loop (1+m) (f last)
    else last
  in
  loop 0 |}
;;

let%expect_test "repeat " =
  wrap ~verbose:false repeat;
  [%expect
    {|
    let repeat n f = let rec loop m last = if m < n then loop (1 + m) (f last)
                                           else last in loop 0
    	~~[2 value bindings]~~>

    let rec loop n f m last = if m < n then loop n f (1 + m) (f last) else last
    let repeat n f = loop n f 0
       |}]
;;

let uuu = {|
let uuu n =
  let rec loop m last = m+n+last in
  loop 0 |}

let%expect_test "uuu: lifting letrec " =
  wrap uuu;
  [%expect
    {|
    let uuu n = let rec loop m last = (m + n) + last in loop 0
    	~~[2 value bindings]~~>

    let rec loop n m last = (m + n) + last
    let uuu n = loop n 0
       |}]
;;
