  $ run () { ../../driver/driver.exe $1 --target anf -o a.ml && cat a.ml; }

  $ run << EOF
  > let rec s f g x = f x (g x)
  > EOF
  let rec s f g x =
    let temp1 = f x  in
      let temp2 = g x  in
        temp1 temp2 

  $ run << EOF
  > let sum x = let (a, b) = x in a+b
  > EOF
  let sum x =
    let temp1 = x in
      let a = field 0 temp1 in
        let b = field 1 temp1 in
          (a + b)

  $ run << EOF
  > let small n =
  >   let half = n  in
  >   (if half then 0 else 1)+1123
  > EOF
  let small n =
    let half = n in
      let temp1 = (if half
                  then 0
                  else 1) in
        (temp1 + 1123)

CPS Factorial
  $ run << EOF
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  let fresh_1 n k m =
    let temp1 = (n * m) in
      k temp1 
  let rec fack n k =
    (if (n = 1)
    then k 1 
    else let temp5 = (n - 1) in
           let temp8 = fresh_1 n k in
             fack temp5 temp8)

CPS Fibonacci
  $ run << EOF
  > let rec fibk n k =
  >  if n<1 then k 1 else fibk (n-1) (fun p -> fibk (n-2) (fun q -> k (p + q)))
  > EOF
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

Polyvariadic currying
  $ run << EOF
  > let fresh f arg rest = f (arg, rest)
  > EOF
  let fresh f arg rest =
    let temp1 = (arg, rest) in
      f temp1 

Polyvariadic currying
  $ run << EOF
  > let two f a b = f (a, b)
  > let succ prev f arg = prev (fun rest -> f (arg,rest))
  > let three = succ two
  > let four = succ three
  > EOF
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

Polyvariadic map
  $ run << EOF
  > let two f (a,b) = (f a, f b)
  > let succ prev f (a, rest) = (f a, prev f rest)
  > let three = succ two
  > let four = succ three
  > let temp = two (fun x -> x) (1,2)
  > EOF
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

  $ run << EOF #-vcc -vanf
  > let foo f x = x
  > EOF
  let foo f x =
    x
