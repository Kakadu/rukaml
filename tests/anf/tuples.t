  $ run () { ../../driver/driver.exe $1 --target anf -o a.ml && cat a.ml; }

Polyvariadic uncurrying
  $ run << EOF
  > let two f (a,b) = f a b
  > let succ prev f (a,rest) = prev (f a) rest
  > let three = succ two
  > let four = succ three
  > EOF
  let two f temp1 =
    let a = field 0 temp1 in
      let b = field 1 temp1 in
        let temp2 = f a  in
          temp2 b 
  let succ prev f temp4 =
    let a = field 0 temp4 in
      let rest = field 1 temp4 in
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
    let temp5 = mydiv a b in
      let u = field 0 temp5 in
        let v = field 1 temp5 in
          (u + v)
