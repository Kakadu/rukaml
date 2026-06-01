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
                       use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "(x,y,z)";
  [%expect
    {|
    (fun tuple1 -> let x = block_nth tuple1 0 in
                     let y = block_nth tuple1 1 in
                       let z = block_nth tuple1 2 in
                         use_pattern_vars_here) |}]
;;

let%expect_test _ =
  test_anf_pat "((x,y),z)";
  [%expect
    {|
    (fun tuple1 -> let field2 = block_nth tuple1 0 in
                     let x = block_nth field2 0 in
                       let y = block_nth field2 1 in
                         let z = block_nth tuple1 1 in
                           use_pattern_vars_here) |}]
;;

let test_anf ?(print_before = false) text =
  reset_gensym ();
  let ( let* ) x f = Result.bind x f in
  match
    let stru = Frontend.Parsing.parse_vb_exn text in
    let vbs = CConv.structure [ Parsetree.Pstr_value stru ] in
    let* _env, stru_typed = Inferencer.structure Typedtree.empty_table vbs in
    (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
    let anf = anf_stru stru_typed in
    if print_before
    then (
      Format.printf "Before simplify:\n%!";
      Format.printf "@[<v>%a@]\n\n%!" pp_stru anf);
    anf |> simplify_stru |> Result.ok
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
        k temp1
    let rec fack n k =
      (if (n = 0)
      then k 1
      else let temp5 = (n - 1) in
             let temp8 = __lifted_lam_1 n k in
               fack temp5 temp8)
    |}]
;;

let%expect_test _ =
  test_anf {| let double = ((let b = 1 in b), 2) |};
  [%expect
    {|
    let double =
      let b = 1 in
        (b, 2)
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
      (__lifted_lam_2, __lifted_lam_3)
    |}]
;;
