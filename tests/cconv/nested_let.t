  $ run () { ../../driver/driver.exe $1 --target cconv -o a.ml && cat a.ml; }

  $ run << EOF
  > let main =
  >   let id x = x in
  >   id 0
  > EOF
  let __lifted_let_1_id x = x
  let main = __lifted_let_1_id 0

  $ run << EOF
  > let f x =
  > let rec aux n y =
  >    let m = aux (n - 1) in
  >    m y in
  >  aux x 0 
  > EOF
  let rec __lifted_let_1_aux n y = let m = __lifted_let_1_aux (n - 1) in 
                                   m y
  let f x = __lifted_let_1_aux x 0
