# Polyvariadic uncurrying
  $ cat << EOF | ./REPL.exe -
  > let two f (a,b) = f a b
  > let succ prev f (a,rest) = prev (f a) rest
  > let three = succ two
  > let four = succ three
  > EOF
  Parsed: let two f (a, b) = f a b
          let succ prev f (a, rest) = prev (f a) rest
          let three = succ two
          let four = succ three
  After CCovv.
  let two: ('_2 -> '_3 -> '_5) -> '_2 * '_3 -> '_5 =
    fun f (a, b) -> (f a) b
  let succ: ('_5 -> '_4 -> '_7) -> ('_3 -> '_5) -> '_3 * '_4 -> '_7 =
    fun prev f (a, rest) -> (prev (f a)) rest
  let three: ('_1 -> '_9 -> '_10 -> '_11) -> '_1 * ('_9 * '_10) -> '_11 =
    succ two
  let four: ('_1 -> '_12 -> '_13 -> '_14 -> '_15) -> '_1 * ('_12 * ('_13 * '_14)) -> '_15 =
    succ three
  After ANF transformation.
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

# let (_,_) = ...
  $ cat << EOF | ./REPL.exe -
  > let mydiv a b = (a+b, a)
  > let f a b = 
  >    let (u,v) = mydiv a b in 
  >    u+v
  > EOF
  Parsed: let mydiv a b = (a + b, a)
          let f a b = let (u, v) = mydiv a b in u + v
  After CCovv.
  let mydiv: int -> int -> int * int =
    fun a b -> ((a + b), a)
  let f: int -> int -> int =
    fun a b -> let (u, v) : int * int = (mydiv a) b in
    u + v
  After ANF transformation.
  let mydiv a b =
    let temp1 = (a + b) in
      (temp1, a)
  let f a b =
    let temp5 = mydiv a b in
      let u = field 0 temp5 in
        let v = field 1 temp5 in
          (u + v)
