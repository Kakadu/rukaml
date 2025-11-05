  $ cat << EOF | ./run.exe -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  Parsed: let rec zed f x = f (zed f) x
          let fac self n = if n = 1 then 1 else n * (self (n - 1))
          let main = zed fac

  $ cat << EOF | ./run.exe  -stru -
  > let id = fun x -> x
  > let idd = fun x -> x
  > let main = (id idd) (id 1)
  > EOF
  Parsed: let id x = x
          let idd x = x
          let main = id idd (id 1)

  $ cat << EOF | ./run.exe  -stru -
  > let rec fix f = f (fix f)
  > EOF
  Parsed: let rec fix f = f (fix f)

  $ cat << EOF | ./run.exe -stru  -
  > let rec fix f = f (fix f)
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = fix fac
  > EOF
  Parsed: let rec fix f = f (fix f)
          let fac self n = if n = 1 then 1 else n * (self (n - 1))
          let main = fix fac

  $ cat << EOF | ./run.exe  -stru -
  > let rec zed f x = f (zed f) x
  > let fac = fun self -> fun n -> if n=1 then 1 else n * (self (n-1))
  > let main = zed fac
  > EOF
  Parsed: let rec zed f x = f (zed f) x
          let fac self n = if n = 1 then 1 else n * (self (n - 1))
          let main = zed fac

  $ cat << EOF | ./run.exe  -e -
  > (fun fix -> fun f -> f (fix f))
  > EOF
  Parsed: (fun fix -> (fun f -> f (fix f)))
  $ cat << EOF | ./run.exe  -e -
  > let rec s f g x = f x (g x) in s
  > EOF
  Parsed: let rec s f g x = f x (g x) in s
  $ cat << EOF | ./run.exe  -stru -
  > let rec fac = fun n -> if n=1 then 1 else n * (fac (n-1))
  > let main = fac
  > EOF
  Parsed: let rec fac n = if n = 1 then 1 else n * (fac (n - 1))
          let main = fac
  $ cat << EOF | ./run.exe  -e -
  > fun f -> fun x -> f (f x)
  > EOF
  Parsed: (fun f -> (fun x -> f (f x)))
  $ cat << EOF | ./run.exe  -e -
  > fun x -> let v = x in v
  > EOF
  Parsed: (fun x -> let v = x in v)
  $ cat << EOF | ./run.exe  -stru -
  > let add = fun x -> fun  y -> x + y
  > let add1 = add 1
  > let main = add1 13
  > EOF
  Parsed: let add x y = x + y
          let add1 = add 1
          let main = add1 13

  $ cat << EOF | ./run.exe  -stru -
  > let add = fun x -> x + x
  > let add1 = add 1
  > let main = add 1
  > EOF
  Parsed: let add x = x + x
          let add1 = add 1
          let main = add 1

  $ cat << EOF | ./run.exe  -stru -
  > let double = fun x -> (x, x)
  > EOF
  Parsed: let double x = (x, x)

patterns
  $ cat << EOF | ./run.exe -pat -
  > (x,y,z)
  > EOF
  Parsed: (x, y, z)

  $ cat << EOF | ./run.exe -pat -
  > x
  > EOF
  Parsed: x

  $ cat << EOF | ./run.exe -stru -
  > let fst (x,y) = x
  > let snd (x,y) = y
  > let swap (x,y) = (y,x)
  > EOF
  Parsed: let fst (x, y) = x
          let snd (x, y) = y
          let swap (x, y) = (y, x)

  $ cat << EOF | ./run.exe -prio -
  >   let (a,b) = swap p in
  >   a+b
  > EOF
  Parsed: let (a, b) = swap p in a + b

  $ cat << EOF | ./run.exe -stru -
  > let swap (a,b) = (b,a)
  > let resum p =
  >   let (a,b) = swap p in
  >   a+b
  > EOF
  Parsed: let swap (a, b) = (b, a)
          let resum p = let (a, b) = swap p in a + b

CPS
  $ cat << EOF | ./run.exe -stru -
  > let rec fack n k =
  >  if n=1 then k 1 else fack (n-1) (fun m -> k (n*m))
  > EOF
  Parsed: let rec fack n k = if n = 1 then k 1 else fack (n - 1) (fun m ->
                                                                  k (n * m))

  $ cat << EOF | ./run.exe -stru -
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
