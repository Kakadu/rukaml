# core_type
  $ cat << EOF | ./run.exe -core-type -
  > int
  Parsed: int

  $ cat << EOF | ./run.exe -core-type -
  > (int)
  Parsed: int

  $ cat << EOF | ./run.exe -core-type -
  > 'a
  Parsed: 'a

  $ cat << EOF | ./run.exe -core-type -
  > ('a)
  Parsed: 'a

  $ cat << EOF | ./run.exe -core-type -
  > 'a * 'b
  Parsed: 'a * 'b

  $ cat << EOF | ./run.exe -core-type -
  > 'a -> 'b -> 'c
  Parsed: 'a -> 'b -> 'c

  $ cat << EOF | ./run.exe -core-type -
  > 'a -> ('b -> 'c)
  Parsed: 'a -> 'b -> 'c

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b) -> 'c
  Parsed: ('a -> 'b) -> 'c

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b) * ('b -> 'a)
  Parsed: ('a -> 'b) * ('b -> 'a)

  $ cat << EOF | ./run.exe -core-type -
  > int list
  Parsed: int list

  $ cat << EOF | ./run.exe -core-type -
  > ('a) list
  Parsed: 'a list

  $ cat << EOF | ./run.exe -core-type -
  > ('a * 'b) list
  Parsed: ('a * 'b) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b -> 'c) list
  Parsed: ('a -> 'b -> 'c) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a, 'b) list
  Parsed: ('a, 'b) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b, 'c * 'd) list
  Parsed: ('a -> 'b, 'c * 'd) list
#

# type declaration
  $ cat << EOF | ./run.exe -stru -
  > type t
  Parsed: type t
          
  $ cat << EOF | ./run.exe -stru -
  > type t = int -> bool
  Parsed: type t = int -> bool
          

  $ cat << EOF | ./run.exe -stru -
  > type 'a my_list = 'a list
  Parsed: type 'a my_list = 'a list
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) pair = 'a * 'b
  Parsed: type ('a, 'b) pair = 'a * 'b
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) arrow = 'a -> 'b
  Parsed: type ('a, 'b) arrow = 'a -> 'b
          
#

# something more complex
  $ cat << EOF | ./run.exe -stru -
  > type 'a option =
  > | Some of 'a
  > | None
  Parsed: type 'a option =
            | Some of 'a
            | None
            
          
  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) arrows =
  >   | Normal of ('a -> 'b)
  >   | Reversed of ('b -> 'a)
  Parsed: type ('a, 'b) arrows =
            | Normal of ('a -> 'b)
            | Reversed of ('b -> 'a)
            
          

  $ cat << EOF | ./run.exe -stru -
  > type 'a list =
  > | Nil
  > | Cons of 'a * 'a list
  Parsed: type 'a list =
            | Nil
            | Cons of 'a * 'a list
            
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) qwe =
  > | Asd of ('a -> ('a -> 'b) -> 'b)
  > | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
  Parsed: type ('a, 'b) qwe =
            | Asd of ('a -> ('a -> 'b) -> 'b)
            | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
            
          

  $ cat << EOF | ./run.exe -stru -
  > type t =
  > | Foo of int
  > | Bar of int list
  > | Qwe of ('a -> 'b) option
  > | Asd of (int list, bool option) map
  > | Zxc of ((int, bool) result, string option list) map list
  > EOF
  Parsed: type t =
            | Foo of int
            | Bar of int list
            | Qwe of ('a -> 'b) option
            | Asd of (int list, bool option) map
            | Zxc of ((int, bool) result, string option list) map list
            
          
#

# "and" chains
  $ cat << EOF | ./run.exe -stru -
  > type a = int
  > and b = bool
  > and c = char
  Parsed: type a = int
          and b = bool
          and c = char
          

  $ cat << EOF | ./run.exe -stru -
  > type 'a box =
  > | Box of 'a
  > 
  > and 't maybe =
  > | Just of 't
  > | Nothing
  > 
  > and name = string
  Parsed: type 'a box =
            | Box of 'a
            
          and 't maybe =
            | Just of 't
            | Nothing
            
          and name = string
          

# invalid input
  $ cat << EOF | ./run.exe -stru -
  > type foo =
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = 123
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = a ->
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = a -> -> a
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = a *
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = a * * a
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type a my_list = a list
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type ''a my_list = ''a list
  Error: : no more choices

  $ cat << EOF | ./run.exe -stru -
  > type '_a my_list = '_a list
  Error: : no more choices

  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > | a
  > | b
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > | A of
  > | B of
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > | A of A
  > | B of B
  Error: : end_of_input

# unsorted
  $ cat << EOF | ./run.exe -stru -
  > type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
  Parsed: type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (int -> int)
  Parsed: type foo = int -> int
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a -> b) * (c -> d)
  Parsed: type foo = (a -> b) * (c -> d)
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a * b) * (c * d)
  Parsed: type foo = (a * b) * (c * d)
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a -> b) -> (c -> d)
  Parsed: type foo = (a -> b) -> c -> d
          
# edge cases

assert int * int and (int * int) ARE NOT equivalent
  $ cat << EOF | ./run.exe -stru -
  > type t = 
  >   | Foo of int * int
  >   | Bar of (int * int)
  Parsed: type t =
            | Foo of int * int
            | Bar of (int * int)
            
          
assert int * int and (int * int) ARE equivalent
  $ cat << EOF | ./run.exe -stru -
  > type t = int * int
  > type t = (int * int)
  Parsed: type t = int * int
          
          type t = int * int
          

should pass
  $ cat << EOF | ./run.exe -stru -
  > type t = ('a -> 'b)
  Parsed: type t = 'a -> 'b
          

should pass
  $ cat << EOF | ./run.exe -stru -
  > type t = 'a -> 'b
  Parsed: type t = 'a -> 'b
          

should pass
  $ cat << EOF | ./run.exe -stru -
  > type t = Foo of ('a -> 'b)
  Parsed: type t =
            | Foo of ('a -> 'b)
            
          

should fail
  $ cat << EOF | ./run.exe -stru -
  > type t = Foo of 'a -> 'b
  Error: : end_of_input

