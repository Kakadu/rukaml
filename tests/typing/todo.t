patterns as let lhs

  $ infer () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ infer << EOF
  > let _ = ()
  > EOF
  let _: unit =
    ()

  $ infer << EOF
  > let () = ()
  > EOF
  let (): unit =
    ()
  $ infer << EOF
  > let (1, 2) = (1, 2)
  > EOF
  let (1, 2): int * int =
    (1, 2)
  $ infer << EOF
  > let (x, y) = (1, 2)
  > EOF
  let (x, y): int * int =
    (1, 2)
  $ infer << EOF
  > let (Some x) = (Some 42)
  > EOF
  infer error: unbound constructror: Some
  [1]

should fail

  $ infer << EOF
  > let rec x = x + 1
  > EOF
  let rec x: int =
    x + 1

  $ infer << EOF
  > let (true, 1) = (1, true)
  > EOF
  let (true, 1): int * bool =
    (1, true)

  $ infer << EOF
  > let (f, g) x = f x + g x
  > EOF
  let __lifted_lam_1: ('_3 -> int) -> ('_3 -> int) -> '_3 -> int =
    fun g f x -> (f x) + (g x)
  let (f, g): '_weak1 -> int =
    (__lifted_lam_1 g) f

  $ infer << EOF
  > let rec f = fun n -> f
  > EOF
  let rec f: '_1 -> '_2 =
    fun n -> f

  $ infer << EOF
  > let rec fac = fun n -> n*fac
  > EOF
  let rec fac: int -> int =
    fun n -> n * fac
