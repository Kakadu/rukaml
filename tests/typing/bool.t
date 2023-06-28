  $ cat << EOF | ./REPL.exe -stru -
  > let foo = fun x -> fun y -> if x then y else true
  > EOF
  Parsed.
  let foo: (bool -> (bool -> bool)) =
    fun x y -> (if x then y else true) 
  $ cat << EOF | ./REPL.exe -stru -
  > let foo = fun x -> if x then 52 else ()
  Parsed.
  Error: unification failed on int and unit
  $ cat << EOF | ./REPL.exe -stru -
  > let foo = fun x -> if x then print 52 else ()
  Parsed.
  let foo: (bool -> unit) =
    fun x -> (if x then print 52 else unit) 
