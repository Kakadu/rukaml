  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }



  $ run << EOF
  > let return x = x
  > 
  > let rec odd n  =
  >   if n=1 then true else if n=0 then false else even (n-1)
  > and even n =
  >   if n = 0 then true else if n=1 then false else return (odd (n-1))
  > EOF
  let return x = x
  let rec even n = if n = 0 then true else if n = 1 then false else return (odd (n - 1))
  and odd n = if n = 1 then true else if n = 0 then false else even (n - 1)
