  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }

  $ run << EOF
  > let main =
  >   let id x = x in
  >   id 0
  > EOF
  let id x = x
  let main = id 0

  $ run << EOF
  > let f x =
  > let rec aux n y =
  >    let m = aux (n - 1) in
  >    m y in
  >  aux x 0 
  > EOF
  let aux n y = let m = aux (n - 1) in m y
  let f x = aux x 0
