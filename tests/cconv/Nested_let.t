  $ cat << EOF | ./REPL.exe -
  > let main =
  >   let id x = x in
  >   id 0
  > EOF
  Parsed: let main = let id x = x in id 0
  After CCovv.
  let id x = x
  let main = id 0

  $ cat << EOF | ./REPL.exe -
  > let f x =
  > let rec aux n y =
  >    let m = aux (n - 1) in
  >    m y in
  >  aux x 0 
  > EOF
  Parsed: let f x = let rec aux n y = let m = aux (n - 1) in m y in aux x 0
  After CCovv.
  let aux n y = let m = aux (n - 1) in m y
  let f x = aux x 0
