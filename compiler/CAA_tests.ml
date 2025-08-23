open CAA
open Frontend.Parsetree
open Frontend.Ident
open CPSLang
open CPSLang.OneACPS

let test_call_ar_anal cps_prog =
  Format.printf "before:\n%a\n" OneACPS.pp_vb cps_prog;
  (* TODO: printer above creates non-closed boxes.
    We need to fix it. Flush is a temporary workaround. *)
  Format.printf "%!";
  call_arity_anal cps_prog |> Format.printf "after:\n%a\n" MACPS.pp_vb;
  Format.printf "%!";
  ANF.reset_gensym ()
;;

let test_call_ar_anal_debug cps_prog =
  Format.printf "before:\n%a\n" OneACPS.pp_vb cps_prog;
  call_arity_anal_debug cps_prog |> Format.printf "after:\n%a\n" MACPS.pp_vb;
  ANF.reset_gensym ()
;;

let v name = name |> of_string
let prog b = NonRecursive, CPVar (v "main"), b
let var_f, var_g, var_x, var_k1 = v "f", v "g", v "x", v "k1"
let var_y, var_k2, var_h, var_t = v "y", v "k2", v "h", v "t"
let var_a, var_k3, var_b, var_k4 = v "a", v "k3", v "b", v "k4"
let var_l, var_q, var_r, var_k5 = v "l", v "q", v "r", v "k5"
let var_z, var_s, var_d, var_u = v "z", v "s", v "d", v "u"
let var_k6, var_jv1, var_k7 = v "k6", v "jv1", v "k7"
let var_fack, var_fibk, var_n = v "fack", v "fibk", v "n"
let var_k8, var_k, var_m, var_j = v "k8", v "k", v "m", v "j"
let var_v, var_w, var_k9, var_c = v "v", v "w", v "k9", v "c"
let one = TConst (PConst_int 1)
let two = TConst (PConst_int 2)
let tr = TConst (PConst_bool true)
let sum t1 t2 = TSafeBinop ("+" |> of_string, t1, t2)
let le t1 t2 = TSafeBinop ("<=" |> of_string, t1, t2)

let%expect_test "expand call" =
  let th = Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, HALT))) in
  let el = Call (UVar var_f, two, Cont (CPVar var_t, Call (UVar var_t, one, HALT))) in
  let rhs =
    let sum = sum (UVar var_x) (UVar var_y) in
    Lam
      ( CPVar var_x
      , var_k1
      , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum))) )
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x + y)) in
              if true then f 1 (fun g -> g 1 (fun x -> x))
              else f 2 (fun t -> t 1 (fun x -> x))
    after:
    let main =
             let f x e1 k1 = k1 (x + e1) in
             if true then f 1 1 (fun x -> x) else f 2 1 (fun x -> x)
    |}]
;;

let%expect_test "not expand call (one of entries has init arity )" =
  let h_let b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_a, var_k3, Call (UVar var_a, one, CVar var_k3))
      , b )
  in
  let th = Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, HALT))) in
  let el =
    Call (UVar var_f, one, Cont (CPVar var_t, Call (UVar var_h, UVar var_t, HALT)))
  in
  let rhs =
    let sum = TSafeBinop ("+" |> of_string, UVar var_x, UVar var_y) in
    Lam
      ( CPVar var_x
      , var_k1
      , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum))) )
  in
  let ite = CIf (tr, th, el) in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, h_let ite));
  [%expect
    {|
    before:
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x + y)) in
              let h a k3 = a 1 k3 in
              if true then f 1 (fun g -> g 1 (fun x -> x))
              else f 1 (fun t -> h t (fun x -> x))
    after:
    let main =
             let f x k1 = k1 (fun y k2 -> k2 (x + y)) in
             if true then f 1 (fun g -> g 1 (fun x -> x))
             else f 1 (fun t -> t 1 (fun x -> x))
    |}]
;;

let%expect_test "expaned but not fully inlined call (argument is big) " =
  let b =
    let th = Call (UVar var_g, one, Cont (CPVar var_t, Call (UVar var_t, one, HALT))) in
    let el = Call (UVar var_g, one, Cont (CPVar var_b, Call (UVar var_b, one, HALT))) in
    let ite = CIf (tr, th, el) in
    Call (UVar var_f, TTuple (one, one, [ one ]), Cont (CPVar var_g, ite))
  in
  let rhs =
    let tuple = TTuple (UVar var_x, UVar var_x, [ sum (UVar var_y) (UVar var_a) ]) in
    let b =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Ret (CVar var_k2, Lam (CPVar var_a, var_k3, Ret (CVar var_k3, tuple))) ) )
    in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, b));
  [%expect
    {|
    before:
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (fun a k3 -> k3 (x, x, y + a))) in
              f
                (1, 1, 1)
                (fun g -> if true then g 1 (fun t -> t 1 (fun x -> x))
                          else g 1 (fun b -> b 1 (fun x -> x)))

    after:
    let main =
             (fun g -> if true then g 1 1 (fun x -> x) else g 1 1 (fun x -> x))
             (fun e1 e2 k3 ->
               (fun x k1 -> k1 (x, x, e1 + e2)) (1, 1, 1) k3)
    |}]
;;

let%expect_test "not expand call (because of sharing loses)" =
  let th =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_q) (UVar var_t))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (HALT, sum (UVar var_z) (UVar var_s))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, sum (UVar var_b) (UVar var_b)))
      , b )
  in
  let rhs =
    let sum = sum (UVar var_l) (UVar var_r) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, sum))) )
        )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let f x k1 =
                let h b k3 = k3 (b + b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l + r))))
                in
              if true
              then f
                     1
                     (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))

              else f
                     1
                     (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (z + s))))

    after:
    let main =
             let f x k1 =
               let h b k3 = k3 (b + b) in
               h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l + r))))
               in
             if true
             then f
                    1
                    (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))

             else f
                    1
                    (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (z + s))))
    |}]
;;

let%expect_test "expand call (no sharing loses since the variables are dead)" =
  let th =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_t) (UVar var_t))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (HALT, sum (UVar var_s) (UVar var_s))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, sum (UVar var_b) (UVar var_b)))
      , b )
  in
  let rhs =
    let sum = sum (UVar var_l) (UVar var_r) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, sum))) )
        )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let f x k1 =
                let h b k3 = k3 (b + b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l + r))))
                in
              if true
              then f
                     1
                     (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (t + t))))

              else f
                     1
                     (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (s + s))))

    after:
    let main =
             let f x e1 k1 =
               let h b k3 = k3 (b + b) in
               h x (fun l -> h e1 (fun r -> k1 (l + r)))
               in
             if true then f 1 1 (fun t -> (fun x -> x) (t + t))
             else f 1 1 (fun s -> (fun x -> x) (s + s))
    |}]
;;

let%expect_test "not expand call (because of sharing loses). LamCall version" =
  let b f =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_q) (UVar var_t))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (f, sum one one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, TTuple (UVar var_x, UVar var_b, [])))
      , b )
  in
  let lam =
    let tuple = TTuple (UVar var_l, UVar var_r, []) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, tuple)))
            ) )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog @@ b lam;
  [%expect
    {|
    before:
    let main =
              (fun x k1 ->
                let h b k3 = k3 (x, b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l, r)))))
                (1 + 1)
                (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))

    after:
    let main =
             (fun x k1 ->
               let h b k3 = k3 (x, b) in
               h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l, r)))))
               (1 + 1)
               (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + t))))
    |}]
;;

let%expect_test
    "expand call (no sharing loses since the variables are dead). LamCall version"
  =
  let b f =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_q) (UVar var_q))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (f, sum one one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let big_def b =
    Let
      ( NonRecursive
      , CPVar var_h
      , Lam (CPVar var_b, var_k3, Ret (CVar var_k3, TTuple (UVar var_x, UVar var_b, [])))
      , b )
  in
  let lam =
    let tuple = TTuple (UVar var_l, UVar var_r, []) in
    let ret =
      Ret
        ( CVar var_k1
        , Lam
            ( CPVar var_y
            , var_k2
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k2, tuple)))
            ) )
    in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  test_call_ar_anal @@ prog @@ b lam;
  [%expect
    {|
    before:
    let main =
              (fun x k1 ->
                let h b k3 = k3 (x, b) in
                h x (fun l -> k1 (fun y k2 -> h y (fun r -> k2 (l, r)))))
                (1 + 1)
                (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (q + q))))

    after:
    let main =
             (fun x k1 ->
               let h b k3 = k3 (x, b) in
               h x (fun l -> h 1 (fun r -> k1 (l, r))))
               (1 + 1)
               (fun q -> (fun x -> x) (q + q))
    |}]
;;

let%expect_test " expand call thanks to fake shared comput. and cheap exprs detection " =
  let rhs =
    let sum var2 = sum (UVar var_x) (UVar var2) in
    let th = Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum var_y))) in
    let el = Ret (CVar var_k1, Lam (CPVar var_a, var_k3, Ret (CVar var_k3, sum var_a))) in
    Lam (CPVar var_x, var_k1, CIf (tr, th, el))
  in
  let th =
    let cont2 = Cont (CPVar var_q, Ret (HALT, sum (UVar var_t) (UVar var_q))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (HALT, sum (UVar var_s) (UVar var_z))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let f x k1 =
                if true then k1 (fun y k2 -> k2 (x + y))
                else k1 (fun a k3 -> k3 (x + a))
                in
              if true
              then f
                     1
                     (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (t + q))))

              else f
                     1
                     (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (s + z))))

    after:
    let main =
             let f x e1 k1 = if true then k1 (x + e1) else k1 (x + e1) in
             if true
             then (fun g -> g 1 (fun t -> g 1 (fun q -> (fun x -> x) (t + q))))
                  (fun e4 k5 ->
                    f 1 e4 k5)

             else (fun g -> g 1 (fun s -> g 1 (fun z -> (fun x -> x) (s + z))))
                  (fun e2 k3 ->
                    f 1 e2 k3)
    |}]
;;

let%expect_test
    " expand call thanks to fake shared comput. and cheap exprs detection. LamCall \
     version"
  =
  let rhs =
    let sum var2 = sum (UVar var_x) (UVar var2) in
    let th = Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum var_y))) in
    let el = Ret (CVar var_k1, Lam (CPVar var_a, var_k3, Ret (CVar var_k3, sum var_a))) in
    Lam (CPVar var_x, var_k1, CIf (tr, th, el))
  in
  let th =
    let cont2 = Cont (CPVar var_q, Ret (CVar var_k4, sum (UVar var_t) (UVar var_q))) in
    let cont1 = Cont (CPVar var_t, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  let el =
    let cont2 = Cont (CPVar var_z, Ret (CVar var_k4, sum (UVar var_s) (UVar var_z))) in
    let cont1 = Cont (CPVar var_s, Call (UVar var_g, one, cont2)) in
    Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, cont1)))
  in
  test_call_ar_anal
  @@ prog
  @@ Call (Lam (CPVar var_f, var_k4, CIf (tr, th, el)), rhs, HALT);
  [%expect
    {|
    before:
    let main =
              (fun f k4 ->
                if true
                then f 1 (fun g -> g 1 (fun t -> g 1 (fun q -> k4 (t + q))))
                else f 1 (fun g -> g 1 (fun s -> g 1 (fun z -> k4 (s + z)))))
                (fun x k1 ->
                  if true then k1 (fun y k2 -> k2 (x + y))
                  else k1 (fun a k3 -> k3 (x + a)))
                (fun x -> x)

    after:
    let main =
             (fun f k4 ->
               if true
               then (fun g -> g 1 (fun t -> g 1 (fun q -> k4 (t + q)))) (fun e3 k4 ->
                                                                        f 1 e3 k4)

               else (fun g -> g 1 (fun s -> g 1 (fun z -> k4 (s + z)))) (fun e1 k2 ->
                                                                        f 1 e1 k2))
               (fun x e5 k1 -> if true then k1 (x + e5) else k1 (x + e5))
               (fun x -> x)
    |}]
;;

let%expect_test "expand lam call argument" =
  let rhs =
    let b =
      Ret
        ( CVar var_k1
        , Lam (CPVar var_y, var_k2, Ret (CVar var_k2, TTuple (UVar var_x, UVar var_y, [])))
        )
    in
    Lam (CPVar var_x, var_k1, b)
  in
  let f =
    Lam
      ( CPVar var_h
      , var_k3
      , Call (UVar var_h, one, Cont (CPVar var_s, Call (UVar var_s, one, CVar var_k3))) )
  in
  let th =
    Call
      ( f
      , Lam
          ( CPVar var_t
          , var_k4
          , Call (UVar var_f, TTuple (one, UVar var_t, []), CVar var_k4) )
      , HALT )
  in
  let el =
    Call
      (UVar var_f, TTuple (one, one, []), Cont (CPVar var_g, Call (UVar var_g, one, HALT)))
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x, y)) in
              if true
              then (fun h k3 -> h 1 (fun s -> s 1 k3))
                     (fun t k4 -> f (1, t) k4)
                     (fun x -> x)
               else f (1, 1) (fun g -> g 1 (fun x -> x))
    after:
    let main =
             let f x e1 k1 = k1 (x, e1) in
             if true then f (1, 1) 1 (fun x -> x) else f (1, 1) 1 (fun x -> x)
    |}]
;;

let%expect_test "expand but not inl lam call argument" =
  let rhs =
    let b =
      Ret
        ( CVar var_k1
        , Lam (CPVar var_y, var_k2, Ret (CVar var_k2, TTuple (UVar var_x, UVar var_y, [])))
        )
    in
    Lam (CPVar var_x, var_k1, b)
  in
  let f =
    let th =
      Call (UVar var_h, one, Cont (CPVar var_s, Call (UVar var_s, one, CVar var_k3)))
    in
    let el =
      Call (UVar var_h, one, Cont (CPVar var_q, Call (UVar var_q, one, CVar var_k3)))
    in
    Lam (CPVar var_h, var_k3, CIf (tr, th, el))
  in
  let th =
    Call
      ( f
      , Lam
          ( CPVar var_t
          , var_k4
          , Call (UVar var_f, TTuple (one, UVar var_t, []), CVar var_k4) )
      , HALT )
  in
  let el =
    Call
      (UVar var_f, TTuple (one, one, []), Cont (CPVar var_g, Call (UVar var_g, one, HALT)))
  in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_f, rhs, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let f x k1 = k1 (fun y k2 -> k2 (x, y)) in
              if true
              then (fun h k3 ->
                     if true then h 1 (fun s -> s 1 k3)
                     else h 1 (fun q -> q 1 k3))
                     (fun t k4 -> f (1, t) k4)
                     (fun x -> x)
               else f (1, 1) (fun g -> g 1 (fun x -> x))
    after:
    let main =
             let f x e1 k1 = k1 (x, e1) in
             if true
             then (fun h k3 -> if true then h 1 1 k3 else h 1 1 k3)
                    (fun t e2 k4 -> f (1, t) e2 k4)
                    (fun x -> x)
              else f (1, 1) 1 (fun x -> x)
    |}]
;;

let%expect_test "expand jv" =
  let sum1 = sum (UVar var_x) (UVar var_y) in
  let sum2 = sum (UVar var_l) (UVar var_r) in
  let th =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_x
          , var_k1
          , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum1))) ) )
  in
  let el =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_l
          , var_k3
          , Ret (CVar var_k1, Lam (CPVar var_r, var_k4, Ret (CVar var_k2, sum2))) ) )
  in
  let cont =
    Cont
      ( CPVar var_f
      , Call (UVar var_f, one, Cont (CPVar var_g, Call (UVar var_g, one, HALT))) )
  in
  test_call_ar_anal @@ prog (Letc (var_jv1, cont, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let jv1 f = f 1 (fun g -> g 1 (fun x -> x)) in if true
                                                             then jv1 (fun x k1 ->
                                                                        k1
                                                                        (fun y k2 ->
                                                                        k2 (x + y)))

                                                             else jv1 (fun l k3 ->
                                                                        k1
                                                                        (fun r k4 ->
                                                                        k2 (l + r)))
    after:
    let main =
             let jv1 f = f 1 1 (fun x -> x) in if true
                                               then jv1 (fun x e2 k1 ->
                                                          k1 (x + e2))

                                               else jv1 (fun l e1 k3 ->
                                                          k1 (l + e1))
    |}]
;;

let%expect_test "dead jv param" =
  let sum1 = sum (UVar var_x) (UVar var_y) in
  let sum2 = sum (UVar var_l) (UVar var_r) in
  let th =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_x
          , var_k1
          , Ret (CVar var_k1, Lam (CPVar var_y, var_k2, Ret (CVar var_k2, sum1))) ) )
  in
  let el =
    Ret
      ( CVar var_jv1
      , Lam
          ( CPVar var_l
          , var_k3
          , Ret (CVar var_k1, Lam (CPVar var_r, var_k4, Ret (CVar var_k2, sum2))) ) )
  in
  let cont = Cont (CPVar var_f, Ret (HALT, sum one one)) in
  test_call_ar_anal @@ prog (Letc (var_jv1, cont, CIf (tr, th, el)));
  [%expect
    {|
    before:
    let main =
              let jv1 f = (fun x -> x) (1 + 1) in if true
                                                  then jv1 (fun x k1 ->
                                                             k1 (fun y k2 ->
                                                                  k2 (x + y)))

                                                  else jv1 (fun l k3 ->
                                                             k1 (fun r k4 ->
                                                                  k2 (l + r)))
    after:
    let main =
             let jv1 f = (fun x -> x) (1 + 1) in if true then jv1 () else jv1 ()
    |}]
;;

let%expect_test
    "barriers in action: only unit-size arguments are inlined when eta-param entry count \
     > 1"
  =
  let big_def b =
    let lam_b =
      Ret
        ( CVar var_k3
        , Lam (CPVar var_b, var_k4, Ret (CVar var_k4, TTuple (UVar var_a, UVar var_b, [])))
        )
    in
    Let (NonRecursive, CPVar var_h, Lam (CPVar var_a, var_k3, lam_b), b)
  in
  let rhs =
    let ite =
      CIf (UVar var_y, Ret (CVar var_k2, UVar var_h), Ret (CVar var_k2, UVar var_h))
    in
    let b = Ret (CVar var_k1, Lam (CPVar var_y, var_k2, ite)) in
    Lam (CPVar var_x, var_k1, big_def b)
  in
  let cont2 =
    Cont
      ( CPVar var_g
      , Call
          ( UVar var_g
          , TTuple (one, two, [])
          , Cont (CPVar var_t, Call (UVar var_t, two, HALT)) ) )
  in
  let cont1 = Cont (CPVar var_r, Call (UVar var_r, le two one, cont2)) in
  let b = Call (UVar var_f, TUnit, cont1) in
  test_call_ar_anal @@ prog @@ Let (NonRecursive, CPVar var_f, rhs, b);
  [%expect
    {|
    before:
    let main =
              let f x k1 =
                let h a k3 = k3 (fun b k4 -> k4 (a, b)) in
                k1 (fun y k2 -> if y then k2 h else k2 h)
                in
              f
                ()
                (fun r -> r
                            (2 <= 1)
                            (fun g -> g (1, 2) (fun t -> t 2 (fun x -> x)))
                         )

    after:
    let main =
             let h a e1 k3 = k3 (a, e1) in
             (fun e2 k2 -> if 2 <= 1 then h e2 2 k2 else h e2 2 k2)
               (1, 2)
               (fun x -> x)
    |}]
;;

let%expect_test "fack" =
  let le_sign, min_sign = "<=" |> of_string, "-" |> of_string in
  let th =
    Ret (CVar var_k1, Lam (CPVar var_k, var_k2, Call (UVar var_k, one, CVar var_k2)))
  in
  let mult = TSafeBinop ("*" |> of_string, UVar var_t, UVar var_n) in
  let el =
    let cont2 = Cont (CPVar var_t, Call (UVar var_k, mult, CVar var_k3)) in
    let cont =
      Cont
        ( CPVar var_h
        , Ret
            (CVar var_k1, Lam (CPVar var_k, var_k3, Call (UVar var_h, UVar var_k, cont2)))
        )
    in
    Call (UVar var_fack, TSafeBinop (min_sign, UVar var_n, one), cont)
  in
  let rhs =
    Lam (CPVar var_n, var_k1, CIf (TSafeBinop (le_sign, UVar var_n, one), th, el))
  in
  let id = Lam (CPVar var_x, var_k4, Ret (CVar var_k4, UVar var_x)) in
  let sec_arg_app = Call (UVar var_g, id, HALT) in
  test_call_ar_anal
  @@ prog
  @@ Let
       ( Recursive
       , CPVar var_fack
       , rhs
       , Call (UVar var_fack, one, Cont (CPVar var_g, sec_arg_app)) );
  [%expect
    {|
    before:
    let main =
              let rec fack n k1 =
                if n <= 1 then k1 (fun k k2 -> k 1 k2)
                else fack
                       (n - 1)
                       (fun h -> k1 (fun k k3 -> h k (fun t -> k (t * n) k3)))

                in
              fack 1 (fun g -> g (fun x k4 -> k4 x) (fun x -> x))
    after:
    let main =
             let rec fack n e1 k1 =
               if n <= 1 then e1 1 k1
               else fack (n - 1) e1 (fun t -> e1 (t * n) k1)
               in
             fack 1 (fun x k4 -> k4 x) (fun x -> x)
    |}]
;;

let%expect_test "fibk" =
  test_call_ar_anal
  @@ Result.get_ok
  @@ CPSConv.cps_conv_program
  @@ Result.get_ok
  @@ Frontend.Parsing.parse_structure
       "let main  = let rec fibk n = if n <=1 then fun k -> k 1 else let h = fibk (n-1) \
        in fun k -> h (fun l -> fibk (n-2) (fun r -> k (l + r)) ) in fibk 1 (fun x -> x)";
  [%expect
    {|
    before:
    let main =
              let rec fibk n k1 =
                if n <= 1 then k1 (fun k k2 -> k 1 k2)
                else fibk
                       (n - 1)
                       (fun t3 -> k1 (fun k k4 ->
                                       t3
                                         (fun l k5 ->
                                           fibk
                                             (n - 2)
                                             (fun t6 -> t6
                                                          (fun r k7 ->
                                                            k (l + r) k7)
                                                          k5
                                                       )
                                           )
                                         k4
                                       ))

                in
              fibk
                1
                (fun t8 -> t8 (fun x k9 -> k9 x) (fun t10 -> (fun x -> x) t10))

    after:
    let main =
             let rec fibk n e11 k1 =
               if n <= 1 then e11 1 k1
               else fibk
                      (n - 1)
                      (fun l k5 -> fibk (n - 2) (fun r k7 -> e11 (l + r) k7) k5)
                      k1

               in
             fibk 1 (fun x k9 -> k9 x) (fun t10 -> (fun x -> x) t10)
    |}]
;;

let%expect_test "branches with diff fin_call_ars" =
  let rhs1 =
    let big_def b =
      Let
        ( NonRecursive
        , CPVar var_h
        , Lam (CPVar var_b, var_k4, Ret (CVar var_k4, sum (UVar var_b) (UVar var_b)))
        , b )
    in
    let sum = sum (UVar var_z) @@ sum (UVar var_l) (UVar var_r) in
    let ret2 =
      Ret
        ( CVar var_k2
        , Lam
            ( CPVar var_y
            , var_k3
            , Call (UVar var_h, UVar var_y, Cont (CPVar var_r, Ret (CVar var_k3, sum))) )
        )
    in
    let ret = Ret (CVar var_k1, Lam (CPVar var_z, var_k2, ret2)) in
    let b = big_def (Call (UVar var_h, UVar var_x, Cont (CPVar var_l, ret))) in
    Lam (CPVar var_x, var_k1, b)
  in
  let rhs2 =
    let big_def b =
      Let
        ( NonRecursive
        , CPVar var_u
        , Lam (CPVar var_a, var_k8, Ret (CVar var_k8, sum (UVar var_a) (UVar var_a)))
        , b )
    in
    let sum = sum (UVar var_j) @@ sum (UVar var_n) (UVar var_d) in
    let ret2 =
      Ret
        ( CVar var_k6
        , Lam
            ( CPVar var_c
            , var_k7
            , Call (UVar var_u, UVar var_c, Cont (CPVar var_d, Ret (CVar var_k7, sum))) )
        )
    in
    let ret = Ret (CVar var_k5, Lam (CPVar var_j, var_k6, ret2)) in
    let b = big_def (Call (UVar var_u, UVar var_s, Cont (CPVar var_n, ret))) in
    Lam (CPVar var_s, var_k5, b)
  in
  let let_f =
    let th = Call (UVar var_q, UVar var_t, CVar var_k9) in
    let el = Call (UVar var_g, UVar var_t, CVar var_k9) in
    let lam_b = CIf (tr, th, el) in
    let b =
      let cont = Cont (CPVar var_m, Ret (HALT, TTuple (UVar var_m, UVar var_g, []))) in
      Call
        ( UVar var_f
        , one
        , Cont
            ( CPVar var_v
            , Call (UVar var_v, one, Cont (CPVar var_w, Call (UVar var_w, one, cont))) )
        )
    in
    Let (Recursive, CPVar var_f, Lam (CPVar var_t, var_k9, lam_b), b)
  in
  let let_q = Let (Recursive, CPVar var_q, rhs2, let_f) in
  test_call_ar_anal @@ prog (Let (NonRecursive, CPVar var_g, rhs1, let_q));
  [%expect
    {|
    before:
    let main =
              let g x k1 =
                let h b k4 = k4 (b + b) in
                h
                  x
                  (fun l -> k1 (fun z k2 ->
                                 k2 (fun y k3 -> h y (fun r -> k3 (z + (l + r))))))

                in
              let rec q s k5 =
                let u a k8 = k8 (a + a) in
                u
                  s
                  (fun n -> k5 (fun j k6 ->
                                 k6 (fun c k7 -> u c (fun d -> k7 (j + (n + d))))))

                in
              let rec f t k9 = if true then q t k9 else g t k9 in
              f 1 (fun v -> v 1 (fun w -> w 1 (fun m -> (fun x -> x) (m, g))))
    after:
    let main =
             let g x k1 =
               let h b k4 = k4 (b + b) in
               h
                 x
                 (fun l -> k1 (fun z k2 ->
                                k2 (fun y k3 -> h y (fun r -> k3 (z + (l + r))))))

               in
             let rec q s e2 e1 k5 =
               let u a k8 = k8 (a + a) in
               u s (fun n -> u e1 (fun d -> k5 (e2 + (n + d))))
               in
             let rec f t k9 =
               if true then k9 (fun e5 k6 -> k6 (fun e3 k4 -> q t e5 e3 k4))
               else g t k9
               in
             f 1 (fun v -> v 1 (fun w -> w 1 (fun m -> (fun x -> x) (m, g))))
    |}]
;;

let%expect_test "dead lam_call_bnd lam par. near the barrier" =
  let lam =
    Lam (CPVar var_x, var_k1, CIf (tr, Ret (CVar var_k1, one), Ret (CVar var_k1, two)))
  in
  let arg_lam =
    let lam2 =
      Lam (CPVar var_b, var_k4, Ret (CVar var_k4, TTuple (UVar var_a, UVar var_b, [])))
    in
    Lam (CPVar var_a, var_k3, Ret (CVar var_k3, lam2))
  in
  let call =
    Call
      (lam, arg_lam, Cont (CPVar var_y, Ret (HALT, TTuple (UVar var_y, UVar var_y, []))))
  in
  test_call_ar_anal @@ prog call;
  [%expect
    {|
    before:
    let main =
              (fun x k1 -> if true then k1 1 else k1 2)
                (fun a k3 -> k3 (fun b k4 -> k4 (a, b)))
                (fun y -> (fun x -> x) (y, y))

    after:
    let main =
             (fun x k1 -> if true then k1 1 else k1 2)
               ()
               (fun y -> (fun x -> x) (y, y))
    |}]
;;
