# some weird edge-cases (which required fixing parser and typechecker)

  $ parse () { ../../driver/driver.exe $1 --target parsetree -o a.ml && cat a.ml; }
  $ infer () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

assert int * int and (int * int) ARE NOT equivalent
  $ parse << EOF
  > type t =
  > | Foo of (int * int)
  > | Bar of int * int
  > EOF
  type t =
    | Foo of (int * int)
    | Bar of int * int
     
  $ infer << EOF
  > type t =
  > | Foo of (int * int)
  > | Bar of int * int
  > EOF
  type t =
    | Foo of (int * int)
    | Bar of int * int

assert int * int and (int * int) ARE equivalent
  $ parse << EOF
  > type t = int * int
  > type t = (int * int)
  type t = int * int 
  type t = int * int 

  $ infer << EOF
  > type t = int * int
  > type t = (int * int)
  type t = int * int
    
  type t = int * int
    
should pass
  $ parse << EOF
  > type t = ('a -> 'b)
  > EOF
  type t = 'a -> 'b 

should pass
  $ parse << EOF
  > type t = 'a -> 'b
  > EOF
  type t = 'a -> 'b 


should pass
  $ parse << EOF
  > type t = Foo of ('a -> 'b)
  > EOF
  type t =
    | Foo of ('a -> 'b)
     

should fail
  $ parse << EOF
  > type t = Foo of 'a -> 'b
  > EOF
  parse error: : end_of_input
  [1]

# nested type constructors
  $ parse << EOF
  > type t =
  > | Foo of int
  > | Bar of int list
  > | Qwe of ('a -> 'b) option
  > | Asd of (int list, bool option) map
  > | Zxc of ((int, bool) result, string option list) map list
  > EOF
  type t =
    | Foo of int
    | Bar of int list
    | Qwe of ('a -> 'b) option
    | Asd of (int list, bool option) map
    | Zxc of ((int, bool) result, string option list) map list
     

