  $ cat << EOF | ./REPL.exe -
  > let rec s f g x = f x (g x)
  > EOF
  Parsed: let rec s f g x = f x (g x)
  After CCovv.
  let rec s: ('_3 -> '_5 -> '_6) -> ('_3 -> '_5) -> '_3 -> '_6 =
    fun f g x -> (f x) (g x)
  After ANF transformation.
  let rec s f g x =
    let temp1 = f x  in
      let temp2 = g x  in
        temp1 temp2 
  $ cat << EOF | ./REPL.exe -
  > let sum x = let (a, b) = x in a+b
  > EOF
  Parsed: let sum x = let (a, b) = x in a + b
  After CCovv.
  let sum: int * int -> int =
    fun x -> let (a, b) : int * int = x in
    a + b
  After ANF transformation.
  let sum x =
    let temp1 = x in
      let a = field 0 temp1 in
        let b = field 1 temp1 in
          (a + b)

  $ cat << EOF | ./REPL.exe -
  > let small n =
  >   let half = n  in
  >   (if half then 0 else 1)+1123
  > EOF
  Parsed: let small n = let half = n in (if half then 0 else 1) + 1123
  After CCovv.
  let small: bool -> int =
    fun n -> let half : bool = n in
    (if half then 0 else 1) + 1123
  After ANF transformation.
  let small n =
    let half = n in
      let temp1 = (if half
                  then 0
                  else 1) in
        (temp1 + 1123)
# CPS Factorial
  $ cat << EOF | ./REPL.exe -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  Parsed: let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m ->
                                                                  k (n * m))
  After CCovv.
  let fresh_1: int -> (int -> '_6) -> int -> '_6 =
    fun n k m -> k (n * m)
  let rec fack: int -> (int -> '_12) -> '_12 =
    fun n k -> (if n = 1 then k 1 else (fack (n - 1)) ((fresh_1 n) k))
  After ANF transformation.
  let fresh_1 n k m =
    let temp1 = (n * m) in
      k temp1 
  let rec fack n k =
    (if (n = 1)
    then k 1 
    else let temp5 = (n - 1) in
           let temp8 = fresh_1 n k in
             fack temp5 temp8)

# CPS Fibonacci
  $ cat << EOF | ./REPL.exe -
  > let rec fibk n k =
  >  if n<1 then k 1 else fibk (n-1) (fun p -> fibk (n-2) (fun q -> k (p + q)))
  > EOF
  Parsed: let rec fibk n k = if n < 1 then k 1 else fibk (n - 1) (fun p ->
                                                                  fibk 
                                                                  (n - 2) 
                                                                  (fun 
                                                                  q -> 
                                                                      k 
                                                                      (p + q)))
  After CCovv.
  let fresh_2: int -> (int -> '_6) -> int -> '_6 =
    fun p k q -> k (p + q)
  let fresh_1: int -> (int -> '_8) -> (int -> (int -> '_8) -> '_11) -> int -> '_11 =
    fun n k fibk p -> (fibk (n - 2)) ((fresh_2 p) k)
  let rec fibk: int -> (int -> '_14) -> '_14 =
    fun n k -> (if (< n) 1 then k 1 else (fibk (n - 1)) (((fresh_1 n) k) fibk))
  After ANF transformation.
  let fresh_2 p k q =
    let temp1 = (p + q) in
      k temp1 
  let fresh_1 n k fibk p =
    let temp3 = (n - 2) in
      let temp4 = fibk temp3  in
        let temp6 = fresh_2 p k in
          temp4 temp6 
  let rec fibk n k =
    (if (n < 1)
    then k 1 
    else let temp10 = (n - 1) in
           let temp14 = fresh_1 n k fibk in
             fibk temp10 temp14)

# Polyvariadic uncurrying
  $ cat << EOF | ./REPL.exe  -
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

# Polyvariadic currying
  $ cat << EOF | ./REPL.exe -
  > let fresh f arg rest = f (arg, rest)
  > EOF
  Parsed: let fresh f arg rest = f (arg, rest)
  After CCovv.
  let fresh: ('_2 * '_3 -> '_4) -> '_2 -> '_3 -> '_4 =
    fun f arg rest -> f (arg, rest)
  After ANF transformation.
  let fresh f arg rest =
    let temp1 = (arg, rest) in
      f temp1 

# Polyvariadic currying
  $ cat << EOF | ./REPL.exe -
  > let two f a b = f (a, b)
  > let succ prev f arg = prev (fun rest -> f (arg,rest))
  > let three = succ two
  > let four = succ three
  > EOF
  Parsed: let two f a b = f (a, b)
          let succ prev f arg = prev (fun rest -> f (arg, rest))
          let three = succ two let four = succ three
  After CCovv.
  let two: ('_2 * '_3 -> '_4) -> '_2 -> '_3 -> '_4 =
    fun f a b -> f (a, b)
  let fresh_1: ('_2 * '_3 -> '_4) -> '_2 -> '_3 -> '_4 =
    fun f arg rest -> f (arg, rest)
  let succ: (('_6 -> '_7) -> '_10) -> ('_3 * '_6 -> '_7) -> '_3 -> '_10 =
    fun prev f arg -> prev ((fresh_1 f) arg)
  let three: ('_1 * ('_6 * '_7) -> '_8) -> '_1 -> '_6 -> '_7 -> '_8 =
    succ two
  let four: ('_1 * ('_9 * ('_10 * '_11)) -> '_12) -> '_1 -> '_9 -> '_10 -> '_11 -> '_12 =
    succ three
  After ANF transformation.
  let two f a b =
    let temp1 = (a, b) in
      f temp1 
  let fresh_1 f arg rest =
    let temp3 = (arg, rest) in
      f temp3 
  let succ prev f arg =
    let temp6 = fresh_1 f arg in
      prev temp6 
  let three =
    succ two 
  let four =
    succ three 

# Polyvariadic map
  $ cat << EOF | ./REPL.exe -
  > let two f (a,b) = (f a, f b)
  > let succ prev f (a, rest) = (f a, prev f rest)
  > let three = succ two
  > let four = succ three
  > let temp = two (fun x -> x) (1,2)
  > EOF
  Parsed: let two f (a, b) = (f a, f b)
          let succ prev f (a, rest) = (f a, prev f rest)
          let three = succ two
          let four = succ three
          let temp = two (fun x -> x) (1, 2)
  After CCovv.
  let two: ('_3 -> '_5) -> '_3 * '_3 -> '_5 * '_5 =
    fun f (a, b) -> ((f a), (f b))
  let succ: (('_3 -> '_5) -> '_4 -> '_7) -> ('_3 -> '_5) -> '_3 * '_4 -> 
            '_5 * '_7 =
    fun prev f (a, rest) -> ((f a), ((prev f) rest))
  let three: ('_9 -> '_10) -> '_9 * ('_9 * '_9) -> '_10 * ('_10 * '_10) =
    succ two
  let four: ('_11 -> '_12) -> '_11 * ('_11 * ('_11 * '_11)) -> '_12 * (
                                                               '_12 * (
                                                               '_12 * '_12)) =
    succ three
  let fresh_1: '_1 -> '_1 =
    fun x -> x
  let temp: int * int =
    (two fresh_1) (1, 2)
  After ANF transformation.
  let two f temp1 =
    let a = field 0 temp1 in
      let b = field 1 temp1 in
        let temp2 = f a  in
          let temp3 = f b  in
            (temp2, temp3)
  let succ prev f temp5 =
    let a = field 0 temp5 in
      let rest = field 1 temp5 in
        let temp6 = f a  in
          let temp7 = prev f  in
            let temp8 = temp7 rest  in
              (temp6, temp8)
  let three =
    succ two 
  let four =
    succ three 
  let fresh_1 x =
    x
  let temp =
    let temp13 = (1, 2) in
      two fresh_1 temp13

  $ cat << EOF | ./REPL.exe - #-vcc -vanf
  > let foo f x = x
  > EOF
  Parsed: let foo f x = x
  After CCovv.
  let foo: '_1 -> '_2 -> '_2 =
    fun f x -> x
  After ANF transformation.
  let foo f x =
    x
