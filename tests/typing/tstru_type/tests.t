  $ cat << EOF | ./run.exe
  > type 'a box = | Box of 'a
  > 
  > let x = Box 1
  result:
  type ([ 0; ]) box =
  | Box of '_0
  
  let x: (int) box =
    Box 1
  
  
  $ cat << EOF | ./run.exe
  > type ('a, 'b) result = 
  >   | Ok of 'a
  >   | Error of 'b
  > 
  > let x = Ok 1
  > let y = Error true
  result:
  type ([ 0; 1; ]) result =
  | Ok of '_0
  | Error of '_1
  
  let x: (int, '_2) result =
    Ok 1
  
  let y: ('_1, bool) result =
    Error true
  
  

  $ cat << EOF | ./run.exe
  > type 'a list = 
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let x = Nil
  > let y = Cons (1, x)
  result:
  type ([ 0; ]) list =
  | Cons of '_0 * ('_0) list
  | Nil
  
  let x: ('_1) list =
    Nil
  
  let y: (int) list =
    Cons (1, x)
  
  

  $ cat << EOF | ./run.exe
  > type ('a, 'b) arrows =
  >   | Normal of 'a -> 'b
  >   | Reversed of 'b -> 'a
  > 
  > let a = Normal (fun x -> x > 0)
  > let b = Reversed (fun x -> x > 0)
  result:
  type ([ 0; 1; ]) arrows =
  | Normal of '_0 -> '_1
  | Reversed of '_1 -> '_0
  
  let a: (int, bool) arrows =
    Normal (fun x -> (> x) 0)
  
  let b: (bool, int) arrows =
    Reversed (fun x -> (> x) 0)
  
  
  $ cat << EOF | ./run.exe
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let x = Pair ((1, 2), (3, 4))
  result:
  type ([ 0; ]) pair =
  | Pair of '_0 * '_0
  
  let x: ((int * int)) pair =
    Pair ((1, 2), (3, 4))
  
  

  $ cat << EOF | ./run.exe
  > type 'a box =
  >   | Box of 'a
  > 
  > let x = Box (fun a -> a + 1)
  > 
  > let y = Box x
  result:
  type ([ 0; ]) box =
  | Box of '_0
  
  let x: ((int -> int)) box =
    Box (fun a -> a + 1)
  
  let y: (((int -> int)) box) box =
    Box x
  
  

# is_pair should have type ('a prod -> bool)
  $ cat << EOF | ./run.exe
  > type 'a prod =
  >   | Pair of 'a * 'a
  >   | Triple of 'a * 'a * 'a
  > 
  > let is_pair x =
  >   match x with
  >   | Pair (a, b) -> true
  >   | Triple (a, b, c) -> false
  result:
  type ([ 0; ]) prod =
  | Pair of '_0 * '_0
  | Triple of '_0 * '_0 * '_0
  
  let is_pair: ('_4) prod -> bool =
    fun x -> match x with
               | Pair (a, b) -> true
               
               | Triple (a, b, c) -> false
               
  
  
#

# sum_prod should have type (int prod -> int)
  $ cat << EOF | ./run.exe
  > type 'a prod =
  >   | Pair of 'a * 'a
  >   | Triple of 'a * 'a * 'a
  > 
  > let sum_prod x =
  >   match x with
  >   | Pair (a, b) -> a + b
  >   | Triple (a, b, c) -> a + b + c
  result:
  type ([ 0; ]) prod =
  | Pair of '_0 * '_0
  | Triple of '_0 * '_0 * '_0
  
  let sum_prod: (int) prod -> int =
    fun x -> match x with
               | Pair (a, b) -> a + b
               
               | Triple (a, b, c) -> (a + b) + c
               
  
  
#
# unification should fail
  $ cat << EOF | ./run.exe
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let x = Pair (1, true)
  inferencer error: unification failed on int and bool
  $ cat << EOF | ./run.exe
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let x = Pair (1, 2, 3)
  inferencer error: unification failed on (int, int) and (int, int, int)

  $ cat << EOF | ./run.exe
  > type 'a list =
  >   | Cons of 'a * 'a list
  >   | Nil
  > 
  > let main =
  >   match Cons (true, Nil) with
  >   | Cons (x, y) -> x + 1
  inferencer error: unification failed on int and bool

  $ cat << EOF | ./run.exe
  > type 'a box =
  >   | Box of 'a
  > 
  > let main =
  >   match (Box true) with
  >   | Box x -> x + 1
  inferencer error: unification failed on int and bool

  $ cat << EOF | ./run.exe
  > type 'a pair =
  >   | Pair of 'a * 'a
  > 
  > let main =
  >   match Pair (1, 2) with
  >   | Pair (a, b, c) -> a + 1
  inferencer error: unification failed on ('_4, '_4) and ('_4, '_4, '_5)
#
