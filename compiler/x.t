
  $ cat << EOF | ./compiler.exe -
  > let rec fac = fun n -> if n=1 then n else n * (fac (n-1))
  > EOF
  let rec fac: int -> int =
    fun n -> (if n = 1 then n else n * (fac (n - 1)))

  $ cat << EOF | ./compiler.exe
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main n = zed fac n
  > EOF
  let rec zed: ((int -> int) -> int -> int) -> int -> int =
    fun f x -> (f (zed f)) x
  let fac: (int -> int) -> int -> int =
    fun self n -> (if n = 1 then 1 else n * (self (n - 1)))
  let main: int -> int =
    fun n -> (zed fac) n
