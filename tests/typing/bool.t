  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ run << EOF
  > let foo = fun x -> fun y -> if x then y else true
  > EOF
  let foo: bool -> bool -> bool =
    fun x y -> (if x then y else true)

  $ run << EOF
  > let foo = fun x -> if x then 52 else ()
  > EOF
  infer error: unification failed on int and unit
  [1]

  $ run << EOF
  > let foo = fun x -> if x then print 52 else ()
  > EOF
  let foo: bool -> unit =
    fun x -> (if x then print 52 else ())
