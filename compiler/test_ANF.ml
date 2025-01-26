open Miniml
open Compile_lib.ANF

let%expect_test _ =
  let ex1 =
    let temp1_id = Ident.of_string "temp1" in
    let f_id = Ident.of_string "f" in
    let x_id = Ident.of_string "x" in
    make_let_nonrec
      temp1_id
      (CAtom (alam f_id (complex_of_atom (alam x_id (complex_of_atom (AVar x_id))))))
      (complex_of_atom (AVar temp1_id))
  in
  Format.printf "%a\n~~>\n%a\n%!" pp ex1 pp (simplify ex1);
  [%expect {|
    let temp1 f = (fun x -> x) in
      temp1
    ~~>
    (fun f x -> x) |}];
  let ex1 =
    let temp1_id = Ident.of_string "temp1" in
    let temp2_id = Ident.of_string "temp2" in
    let f_id = Ident.of_string "f" in
    let x_id = Ident.of_string "x" in
    make_let_nonrec
      temp1_id
      (CAtom
         (alam
            f_id
            (make_let_nonrec
               temp2_id
               (CAtom (alam x_id (complex_of_atom (AVar x_id))))
               (complex_of_atom (AVar temp2_id)))))
      (complex_of_atom (AVar temp1_id))
  in
  Format.printf "%a\n~~>\n%a\n%!" pp ex1 pp (simplify ex1);
  [%expect
    {|
    let temp1 f = let temp2 x = x in
                    temp2 in
      temp1
    ~~>
    (fun f x -> x) |}]
;;

let%expect_test _ =
  test_anf_pat "x";
  [%expect {| (fun x -> use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "(x,y)";
  [%expect
    {|
    (fun temp1 -> let x = field 0 temp1 in
                    let y = field 1 temp1 in
                      use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "(x,y,z)";
  [%expect
    {|
    (fun temp1 -> let x = field 0 temp1 in
                    let y = field 1 temp1 in
                      let z = field 2 temp1 in
                        use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "((x,y),z)";
  [%expect
    {|
    (fun temp1 -> let temp2 = field 0 temp1 in
                    let x = field 0 temp2 in
                      let y = field 1 temp2 in
                        let z = field 1 temp1 in
                          use_pattern_vars_here) |}]
;;

let test_anf text =
  reset_gensym ();
  let ( let* ) x f = Result.bind x f in
  match
    let stru = Miniml.Parsing.parse_vb_exn text in
    let vbs = CConv.structure [ stru ] in
    let* vbs_typed = Inferencer.structure vbs in
    (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
    anf_stru vbs_typed |> List.map simplify_vb |> Result.ok
  with
  | Result.Error err -> Format.printf "%a\n%!" Inferencer.pp_error err
  | Ok anf -> Format.printf "@[<v>%a@]\n%!" (Format.pp_print_list pp_vb) anf
;;

let%expect_test "CPS factorial" =
  test_anf
    {| let rec fack n k =
    if n=0 then k 1
    else fack (n-1) (fun p -> k (p*n)) |};
  [%expect
    {|
    let fresh_1 n k p =
      let temp1 = (p * n) in
        k temp1
    let rec fack n k =
      let temp3 = (n = 0) in
        (if temp3
        then k 1
        else let temp5 = (n - 1) in
               let temp6 = fack temp5  in
                 let temp7 = fresh_1 n  in
                   let temp8 = temp7 k  in
                     temp6 temp8 ) |}]
;;

let%expect_test _ =
  test_anf {| let double = ((let b = 1 in b), 2) |};
  [%expect {|
    let double =
      let b = 1 in
        (b, 2) |}]
;;

let%expect_test _ =
  test_anf {| let foo = ((fun x -> x), (fun y -> y)) |};
  [%expect
    {|
    let fresh_2 x =
      x
    let fresh_3 y =
      y
    let foo =
      (fresh_2, fresh_3)
     |}]
;;
