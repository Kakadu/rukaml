open Frontend
open CPSConv

let test_cps = CPSConv.test_cps

let%expect_test "cps simple func" =
  test_cps {| let double x = 2 * x|};
  [%expect
    {|
    let double x k1 =
      k1 (2 * x)
    |}]
;;

let%expect_test "cps simple prog" =
  test_cps
    {| let double x = 2 * x
  let main = double (double 3)|};
  [%expect
    {|
    let double x k1 =
      k1 (2 * x)
    let main =
      double 3 (fun t2 -> double t2 (fun x -> x))
    |}]
;;

(* we don't inline vbs anymore!*)
let%expect_test "cps prog inlining" =
  test_cps
    {|let y = 3
  let double x = 2 * x
  let main = double (y + y)|};
  [%expect
    {|
    let y =
      3
    let double x k1 =
      k1 (2 * x)
    let main =
      double (y + y) (fun x -> x)
    |}]
;;

let%expect_test "cps rec func (inlining banned)" =
  test_cps {| let y = let rec t x = t 1 in t 2|};
  [%expect
    {|
    let y =
      let rec t x k1 = t 1 k1 in
      t 2 (fun x -> x)
    |}]
;;

let%expect_test "cps eta" =
  test_cps
    {|
   let main = let g x = x in (fun x -> g x) g|};
  [%expect
    {|
    let main =
      let g x k1 = k1 x in
      g g (fun x -> x)
    |}]
;;

let%expect_test "cps eta let" =
  test_cps
    {|
   let main = let g x = x in let f y = g y in f (g 0)|};
  [%expect
    {|
    let main =
      let g x k1 = k1 x in
      g 0 (fun t2 -> g t2 (fun x -> x))
    |}]
;;

let%expect_test "cps fac" =
  test_cps {| let rec fac n = if n = 1 then 1 else fac (n-1) * n|};
  [%expect
    {|
    let rec fac n k1 =
      if n = 1 then k1 1 else fac (n - 1) (fun t2 -> k1 (t2 * n))
    |}]
;;

let%expect_test "cps fib" =
  test_cps
    {|
  let rec  fib n =
if n < 2 then n else fib (n - 1) + fib (n - 2)
  |};
  [%expect
    {|
    let rec fib n k1 =
      if n < 2 then k1 n
      else fib (n - 1) (fun t2 -> fib (n - 2) (fun t3 -> k1 (t2 + t3)))
    |}]
;;

let%expect_test "cps complex branching" =
  test_cps {| let x f = 1 + if (f 2) then 3 else 5|};
  [%expect
    {|
    let x f k1 =
      f 2 (fun t2 -> let jv3 t4 = k1 (1 + t4) in if t2 then jv3 3 else jv3 5)
    |}]
;;

let%expect_test "cps print" =
  test_cps {|let main  = (fun z -> 1) (print 0) |};
  [%expect
    {|
    let main =
      let x1 = print 0 in (fun z -> (fun x -> x) 1) x1
    |}]
;;

let%expect_test "cps print alias " =
  test_cps {| let f = let p = print in let z = p 0 in z + 1|};
  [%expect
    {|
    let f =
      let z = print 0 in (fun x -> x) (z + 1)
    |}]
;;

let%expect_test "cps one ref arg-binop" =
  test_cps {| let f = let g x = x + 1 in g (2 * 2)   |};
  [%expect
    {|
    let f =
      (2 * 2) + 1
    |}]
;;

let%expect_test "cps one ref binop" =
  test_cps {| let f g = let x = 2 * 2 in g x |};
  [%expect
    {|
    let f g k1 =
      g (2 * 2) k1
    |}]
;;

let%expect_test "cps mult refs arg-const" =
  test_cps {| let f g = let x = 2 in g x x|};
  [%expect
    {|
    let f g k1 =
      g 2 (fun t2 -> t2 2 k1)
    |}]
;;

let%expect_test "cps  mult refs arg-binop (inlining banned)" =
  test_cps {| let f g = let x = 2 * 2 in g x x|};
  [%expect
    {|
    let f g k1 =
      let x = 2 * 2 in g x (fun t2 -> t2 x k1)
    |}]
;;

let%expect_test "cps complex tuple-arg" =
  test_cps {| let f g = g (g 3, 1)|};
  [%expect
    {|
    let f g k1 =
      g 3 (fun t2 -> g (t2, 1) k1)
    |}]
;;

let%expect_test "cps ptuple" =
  test_cps {| let f (x,y) = x + y|};
  [%expect
    {|
    let f (x, y) k1 =
      k1 (x + y)
    |}]
;;

let%expect_test "cps free vars" =
  test_cps {| let f x = x + y + z|};
  [%expect
    {|
  Variables are not in scope:
  z
  y
|}]
;;

let%expect_test "cps func in func" =
  test_cps {| let z = let rec g y = y in  let f = fun x -> g in f 2 3|};
  [%expect
    {|
    let z =
      let rec g y k1 = k1 y in
      (fun x -> g 3 (fun x -> x)) 2
    |}]
;;

let%expect_test "cps not allowed let rec" =
  test_cps {| let main  = let rec  x =  x 0 in 0|};
  [%expect
    {|  (x 0): This kind of expression is not allowed as right-hand side of `let rec'
|}]
;;

let%expect_test "cps not allowed let rec (top-level)" =
  test_cps {| let rec  x =  x 0|};
  [%expect
    {|  (x 0): This kind of expression is not allowed as right-hand side of `let rec'
|}]
;;

let%expect_test "cps not allowed let rec lambda complex" =
  test_cps {| let main  = let rec x = (fun z -> (fun y -> x )) 0 in 0|};
  [%expect
    {|  ((fun z -> (fun y -> x)) 0): This kind of expression is not allowed as right-hand side of `let rec'
|}]
;;

let%expect_test "cps not allowed let rec lambda complex (top-level)" =
  test_cps {|let rec x = (fun z -> (fun y -> x )) 0 |};
  [%expect
    {|  ((fun z -> (fun y -> x)) 0): This kind of expression is not allowed as right-hand side of `let rec'
|}]
;;

let%expect_test "cps fake rec" =
  test_cps {| let main  = let rec x = (fun y -> 8) 12 in 0|};
  [%expect
    {|
    let main =
      (fun y -> let rec x = 8 in (fun x -> x) 0) 12
    |}]
;;

let%expect_test "print-arg when not-inlined" =
  test_cps
    {| let revapply a f = f a
let apply f a = f a

let main =
  let z1 = revapply 21 print in
  let z2 = apply print 22 in
  0|};
  [%expect
    {|
    let revapply a k1 =
      k1 (fun f k2 -> f a k2)
    let apply f k3 =
      k3 f
    let main =
      revapply
        21
        (fun t5 -> t5
                     (fun x6 k7 -> let x14 = print x6 in k7 x14)
                     (fun t8 -> let z1 = t8 in apply
                                                 (fun x9 k10 ->
                                                   let x13 = print x9 in
                                                   k10 x13)
                                                 (fun t11 -> t11
                                                               22
                                                               (fun t12 ->
                                                                let z2 = t12 in
                                                                (fun x -> x) 0)
                                                            )
                                               )
                  )
    |}]
;;
