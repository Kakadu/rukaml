  $ cat << EOF | ./REPL.exe -
  > let rec s f g x = f x (g x)
  > EOF
  Parsed: let rec s f g x = f x (g x)
  After CCovv.
  let rec s: (('_3 -> ('_5 -> '_6)) -> (('_3 -> '_5) -> ('_3 -> '_6))) =
    fun f g x -> (f x) (g x)
  After ANF transformation.
  let rec s =
    (fun f g x -> let temp4 = f x  in
                    let temp5 = g x  in
                      temp4 temp5 )
# Zed combinator should trigger occurs check
  $ cat << EOF | ./REPL.exe -
  > let main = fun f -> (fun x -> f (fun v -> x x v)) (fun x -> f (fun v -> x x v))
  > EOF
  Parsed: let main f = (fun x -> f (fun v -> x x v)) (fun x -> f (fun v ->
                                                                  x x v))
  Error: Occurs check failed
  $ cat << EOF | ./REPL.exe -
  > let rec f = fun n -> f
  > EOF
  Parsed: let rec f n = f
  Error: Occurs check failed

  $ cat << EOF | ./REPL.exe -
  > let rec fac = fun n -> n*fac
  > EOF
  Parsed: let rec fac n = n * fac
  Error: unification failed on int and (int -> int)

  $ cat << EOF | ./REPL.exe -
  > let sum x = let (a, b) = x in a+b
  > EOF
  Parsed: let sum x = let (a, b) = x in a + b
  After CCovv.
  let sum: ((int, int) -> int) =
    fun x -> let (a, b) : (int, int) = x in
    a + b
  After ANF transformation.
  let sum =
    (fun x -> let (a, b) = x in
                (a + b))

  $ cat << EOF | ./REPL.exe -
  > let small n =
  >   let half = n  in
  >   (if half then 0 else 1)+1123
  > EOF
  Parsed: let small n = let half = n in (if half then 0 else 1) + 1123
  After CCovv.
  let half: ('_1 -> '_1) =
    fun n -> n
  let small: (bool -> int) =
    fun n -> (if half n then 0 else 1) + 1123
  After ANF transformation.
  let half =
    (fun n -> n)
  let small =
    (fun n -> let temp3 = half n  in
                let temp4 = (if temp3
                            then 0
                            else 1) in
                  (temp4 + 1123))
# CPS Factorial
  $ cat << EOF | ./REPL.exe -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  Parsed: let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m ->
                                                                  k (n * m))
  After CCovv.
  let fresh_1: (int -> ((int -> '_6) -> (int -> '_6))) =
    fun n k m -> k (n * m)
  let rec fack: (int -> ((int -> '_12) -> '_12)) =
    fun n k -> (if n = 1 then k 1 else (fack (n - 1)) ((fresh_1 n) k))
  After ANF transformation.
  let fresh_1 =
    (fun n k m -> let temp4 = (n * m) in
                    k temp4 )
  let rec fack =
    (fun n k -> let temp8 = (n = 1) in
                  (if temp8
                  then k 1 
                  else let temp10 = (n - 1) in
                         let temp11 = fack temp10  in
                           let temp12 = fresh_1 n  in
                             let temp13 = temp12 k  in
                               temp11 temp13 ))

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
  let fresh_2: (int -> ((int -> '_6) -> (int -> '_6))) =
    fun p k q -> k (p + q)
  let fresh_1: (int -> ((int -> '_8) -> ((int -> ((int -> '_8) -> '_11)) -> (int -> '_11)))) =
    fun n k fibk p -> (fibk (n - 2)) ((fresh_2 p) k)
  let rec fibk: (int -> ((int -> '_14) -> '_14)) =
    fun n k -> (if (< n) 1 then k 1 else (fibk (n - 1)) (((fresh_1 n) k) fibk))
  After ANF transformation.
  let fresh_2 =
    (fun p k q -> let temp4 = (p + q) in
                    k temp4 )
  let fresh_1 =
    (fun n k fibk -> (fun p -> let temp10 = (n - 2) in
                                 let temp11 = fibk temp10  in
                                   let temp12 = fresh_2 p  in
                                     let temp13 = temp12 k  in
                                       temp11 temp13 ))
  let rec fibk =
    (fun n k -> let temp17 = (n < 1) in
                  (if temp17
                  then k 1 
                  else let temp19 = (n - 1) in
                         let temp20 = fibk temp19  in
                           let temp21 = fresh_1 n  in
                             let temp22 = temp21 k  in
                               let temp23 = temp22 fibk  in
                                 temp20 temp23 ))

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
  let two: (('_2 -> ('_3 -> '_5)) -> (('_2, '_3) -> '_5)) =
    fun f (a, b) -> (f a) b
  let succ: (('_5 -> ('_4 -> '_7)) -> (('_3 -> '_5) -> (('_3, '_4) -> '_7))) =
    fun prev f (a, rest) -> (prev (f a)) rest
  let three: (('_1 -> ('_9 -> ('_10 -> '_11))) -> (('_1, ('_9, '_10)) -> '_11)) =
    succ two
  let four: (('_1 -> ('_12 -> ('_13 -> ('_14 -> '_15)))) -> (('_1, ('_12, 
                                                                   ('_13, '_14))) -> '_15)) =
    succ three
  After ANF transformation.
  let two =
    (fun f (a, b) -> let temp3 = f a  in
                       temp3 b )
  let succ =
    (fun prev f (a, rest) -> let temp8 = f a  in
                               let temp9 = prev temp8  in
                                 temp9 rest )
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
  let fresh: ((('_2, '_3) -> '_4) -> ('_2 -> ('_3 -> '_4))) =
    fun f arg rest -> f (arg, rest)
  After ANF transformation.
  let fresh =
    (fun f arg rest -> let temp4 = (arg, rest) in
                         f temp4 )

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
  let two: ((('_2, '_3) -> '_4) -> ('_2 -> ('_3 -> '_4))) =
    fun f a b -> f (a, b)
  let fresh_1: ((('_2, '_3) -> '_4) -> ('_2 -> ('_3 -> '_4))) =
    fun f arg rest -> f (arg, rest)
  let succ: ((('_6 -> '_7) -> '_10) -> ((('_3, '_6) -> '_7) -> ('_3 -> '_10))) =
    fun prev f arg -> prev ((fresh_1 f) arg)
  let three: ((('_1, ('_6, '_7)) -> '_8) -> ('_1 -> ('_6 -> ('_7 -> '_8)))) =
    succ two
  let four: ((('_1, ('_9, ('_10, '_11))) -> '_12) -> ('_1 -> ('_9 -> ('_10 -> ('_11 -> '_12))))) =
    succ three
  After ANF transformation.
  let two =
    (fun f a b -> let temp4 = (a, b) in
                    f temp4 )
  let fresh_1 =
    (fun f arg rest -> let temp9 = (arg, rest) in
                         f temp9 )
  let succ =
    (fun prev f arg -> let temp14 = fresh_1 f  in
                         let temp15 = temp14 arg  in
                           prev temp15 )
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
  let two: (('_3 -> '_5) -> (('_3, '_3) -> ('_5, '_5))) =
    fun f (a, b) -> ((f a), (f b))
  let succ: ((('_3 -> '_5) -> ('_4 -> '_7)) -> (('_3 -> '_5) -> (('_3, '_4) -> 
            ('_5, '_7)))) =
    fun prev f (a, rest) -> ((f a), ((prev f) rest))
  let three: (('_9 -> '_10) -> (('_9, ('_9, '_9)) -> ('_10, ('_10, '_10)))) =
    succ two
  let four: (('_11 -> '_12) -> (('_11, ('_11, ('_11, '_11))) -> ('_12, 
                                                                ('_12, 
                                                                ('_12, '_12))))) =
    succ three
  let fresh_1: ('_1 -> '_1) =
    fun x -> x
  let temp: (int, int) =
    (two fresh_1) (1, 2)
  After ANF transformation.
  let two =
    (fun f (a, b) -> let temp3 = f a  in
                       let temp4 = f b  in
                         (temp3, temp4))
  let succ =
    (fun prev f (a, rest) -> let temp9 = f a  in
                               let temp10 = prev f  in
                                 let temp11 = temp10 rest  in
                                   (temp9, temp11))
  let three =
    succ two 
  let four =
    succ three 
  let fresh_1 =
    (fun x -> x)
  let temp =
    let temp16 = two fresh_1  in
      let temp17 = (1, 2) in
        temp16 temp17 
