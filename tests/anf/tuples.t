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
  let two f (a, b) =
    let temp3 = f a  in
      temp3 b 
  let succ prev f (a, rest) =
    let temp8 = f a  in
      let temp9 = prev temp8  in
        temp9 rest 
  let three =
    succ two 
  let four =
    succ three 
