  $ run () { ../../driver/driver.exe $1 --target anf -o a.ml && cat a.ml; }

Polyvariadic uncurrying
  $ run << EOF
  > let two f (a,b) = f a b
  > let succ prev f (a,rest) = prev (f a) rest
  > let three = succ two
  > let four = succ three
  > EOF
  let two f tuple1 =
    let a = block_nth tuple1 0 in
      let b = block_nth tuple1 1 in
        let temp2 = f a  in
          temp2 b 
  let succ prev f tuple4 =
    let a = block_nth tuple4 0 in
      let rest = block_nth tuple4 1 in
        let temp5 = f a  in
          let temp6 = prev temp5  in
            temp6 rest 
  let three =
    succ two 
  let four =
    succ three 

let (_,_) = ...
  $ run << EOF
  > let mydiv a b = (a+b, a)
  > let f a b = 
  >    let (u,v) = mydiv a b in 
  >    u+v
  > EOF
  let mydiv a b =
    let temp1 = (a + b) in
      (temp1, a)
  let f a b =
    let tuple5 = mydiv a b in
      let u = block_nth tuple5 0 in
        let v = block_nth tuple5 1 in
          (u + v)
