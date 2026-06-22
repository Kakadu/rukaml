open Frontend
open ANF

let complex_of_atom x = EComplex (CAtom x)
let alam name e = ALam (Apat_var name, e)
let elam name e = complex_of_atom (alam name e)

let test_anf_pat text =
  reset_gensym ();
  match
    let pat = Frontend.Parsing.parse_pat_exn text in
    anf_pat ~kbefore:elam (Typedtree.of_untyped_pattern pat) (fun _name ->
      complex_of_atom (AVar (Ident.of_string "use_pattern_vars_here")))
    |> Result.ok
  with
  | Result.Error err -> Format.printf "%a\n%!" Inferencer.pp_error err
  | Ok e -> Format.printf "@[<v>%a@]\n%!" pp e
;;

let%expect_test _ =
  test_anf_pat "x";
  [%expect {| (fun x -> use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "(x,y)";
  [%expect
    {|
    (fun tuple1 -> let x = block_nth tuple1 0 in
                   let y = block_nth tuple1 1 in
                   use_pattern_vars_here)
    |}]
;;

let%expect_test _ =
  test_anf_pat "(x,y,z)";
  [%expect
    {|
    (fun tuple1 -> let x = block_nth tuple1 0 in
                   let y = block_nth tuple1 1 in
                   let z = block_nth tuple1 2 in
                   use_pattern_vars_here)
    |}]
;;

let%expect_test _ =
  test_anf_pat "((x,y),z)";
  [%expect
    {|
    (fun tuple1 -> let field2 = block_nth tuple1 0 in
                   let x = block_nth field2 0 in
                   let y = block_nth field2 1 in
                   let z = block_nth tuple1 1 in
                   use_pattern_vars_here)
    |}]
;;

let test_anf ?(simplify = false) ?(print_before = false) text =
  reset_gensym ();
  let simplify_anf = if simplify then simplify_stru else Fun.id in
  let ( let* ) x f = Result.bind x f in
  match
    let stru = Frontend.Parsing.parse_vb_exn text in
    let vbs = CConv.structure [ Parsetree.Pstr_value stru ] in
    let* _env, stru_typed = Inferencer.structure Typedtree.empty_table vbs in
    let anf = anf_stru stru_typed in
    if print_before
    then (
      Format.printf "Before simplify:\n%!";
      Format.printf "@[<v>%a@]\n\n%!" pp_stru anf);
    anf |> simplify_anf |> Result.ok
  with
  | Result.Error err -> Format.printf "%a\n%!" Inferencer.pp_error err
  | Ok anf -> Format.printf "@[<v>%a@]\n%!" pp_stru anf
;;

let%expect_test "CPS factorial" =
  test_anf
    {|
  let rec fack n k =
    if n = 0 then k 1
    else fack (n-1) (fun p -> k (p*n)) |};
  [%expect
    {|
    let __lifted_lam_1 n k p =
      let temp1 = (p * n) in
      let temp2 = k temp1  in
      temp2
    let rec fack n k =
      let temp3 = (n = 0) in
      let temp4 = (if temp3
                  then let temp10 = k 1  in
                       temp10
                  else let temp5 = (n - 1) in
                       let temp6 = fack temp5  in
                       let temp7 = __lifted_lam_1 n  in
                       let temp8 = temp7 k  in
                       let temp9 = temp6 temp8  in
                       temp9) in
        temp4
    |}]
;;

let%expect_test _ =
  test_anf {| let double = ((let b = 1 in b), 2) |};
  [%expect
    {|
    let double =
      let b = 1 in
      let temp1 = (b, 2) in
        temp1
    |}]
;;

let%expect_test _ =
  test_anf {| let foo = ((fun x -> x), (fun y -> y)) |};
  [%expect
    {|
    let __lifted_lam_2 x =
      x
    let __lifted_lam_3 y =
      y
    let foo =
      let temp1 = (__lifted_lam_2, __lifted_lam_3) in
        temp1
    |}]
;;

let%expect_test "Check string literal is put to separate let" =
  test_anf
    ~simplify:true
    {| let main =
         let t =  output_string stdout "hello world!" in
         0 |};
  [%expect
    {|
    let main =
      let temp2 = output_string stdout "hello world!" in
      0
    |}]
;;

let%expect_test "Check string literal is put to separate let" =
  (* ANF.set_logging true; *)
  test_anf
    ~simplify:true
    {| let main =
         let is_keyword s =
            match s with
            | "true" -> true
            | "false" -> true
            | _ -> false in
         0 |};
  [%expect
    {|
    let __lifted_let_4_is_keyword s =
      let temp4 = (s = "true") in
      (if temp4
      then 1
      else let temp3 = (s = "false") in
           (if temp3
           then 1
           else 0))
    let main =
      0
    |}];
  ANF.set_logging false
;;

let%expect_test "Test char equality" =
  test_anf ~simplify:true "let main c =    if c = '0' then 1 else 2";
  [%expect
    {|
    let main c =
      (if (c = '0')
      then 1
      else 2)
    |}]
;;

let%expect_test _ =
  test_anf (* ~print_before:true *)
    ~simplify:true
    {|

    let pp_tuple pp_item oc x =
      fprintf oc ", %a" pp_item x
    |};
  [%expect
    {|
    let pp_tuple pp_item oc x =
      let temp2 = fprintf oc ", %a" in
      let temp3 = temp2 pp_item  in
      temp3 x
    |}]
;;

let%expect_test "Substitution of variable renames" =
  test_anf
    ~simplify:true
    {|
    let f x =
      let z = x in
      z+1
    |};
  [%expect
    {|
    let f x =
      (x + 1)
    |}]
;;

let%expect_test "Substitution of variable renames" =
  test_anf
    ~simplify:true
    {|

let test_keywords () =
  let is_keyword k = true in
    let sq = "let2" in
    if is_keyword sq then 0
    else
      let () = output_string stdout sq in
      1

    |};
  [%expect
    {|
    let __lifted_let_5_is_keyword k =
      true
    let test_keywords () =
      let sq = "let2" in
      let temp1 = __lifted_let_5_is_keyword sq  in
      (if temp1
      then 0
      else let temp4 = output_string stdout sq in
           let () = temp4 in
           1)
    |}]
;;

let%expect_test "ANF function with unit args" =
  test_anf
    ~simplify:true
    {|

let test_keywords () () x = x+1

    |};
  [%expect
    {|
    let test_keywords () () x =
      (x + 1)
    |}]
;;

let%expect_test "... simplify match list " =
  test_anf
    ~simplify:true
    {|
      let f xs = match xs with
          [] -> let w = 1 in 1
        | h::tl -> let z = 5 in  2
|};
  [%expect
    {|
    let f xs =
      let temp2 = block_tag xs  in
      (if (temp2 = 0)
      then let w = 1 in
           1
      else let h = block_nth xs 0 in
           let tl = block_nth xs 1 in
           let z = 5 in
           2)
    |}]
;;

let%expect_test "...  " =
  (* ANF.set_logging true; *)
  test_anf
    ~simplify:true
    {|
  let main =
  let s0 = [ 0 ] in
  let s1 = [ s0 ] in
  let s2 = [ s1 ] in
  match s2 with
  | [] -> s2
  | x :: xs -> s2
|};
  [%expect
    {|
    let main =
      let temp1 = Constr_0 in
      let temp2 = (Constr_1 (0, temp1)) in
      let temp3 = Constr_0 in
      let temp4 = (Constr_1 (temp2, temp3)) in
      let temp5 = Constr_0 in
      let temp6 = (Constr_1 (temp4, temp5)) in
      let temp8 = block_tag temp6  in
      (if (temp8 = 0)
      then temp6
      else let x = block_nth temp6 0 in
           let xs = block_nth temp6 1 in
           temp6)
    |}]
;;
