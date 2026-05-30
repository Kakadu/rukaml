# core_type
  $ cat << EOF | ./run.exe -core-type -
  > int
  > EOF
  Parsed: int

  $ cat << EOF | ./run.exe -core-type -
  > (int)
  > EOF
  Parsed: int

  $ cat << EOF | ./run.exe -core-type -
  > 'a
  > EOF
  Parsed: 'a

  $ cat << EOF | ./run.exe -core-type -
  > ('a)
  > EOF
  Parsed: 'a

  $ cat << EOF | ./run.exe -core-type -
  > 'a * 'b
  > EOF
  Parsed: ('a * 'b)

  $ cat << EOF | ./run.exe -core-type -
  > 'a -> 'b
  > EOF
  Parsed: ('a -> 'b)

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b) * ('b -> 'a)
  > EOF
  Parsed: (('a -> 'b) * ('b -> 'a))

  $ cat << EOF | ./run.exe -core-type -
  > int list
  > EOF
  Parsed: (int) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a) list
  > EOF
  Parsed: 'a list

  $ cat << EOF | ./run.exe -core-type -
  > ('a * 'b) list
  > EOF
  Parsed: (('a * 'b)) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b -> 'c) list
  > EOF
  Parsed: (('a -> ('b -> 'c))) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a, 'b) list
  > EOF
  Parsed: ('a, 'b) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b, 'c * 'd) list
  > EOF
  Parsed: (('a -> 'b), ('c * 'd)) list
#

# type declaration
  $ cat << EOF | ./run.exe -stru -
  > type t = int
  > EOF
  Parsed: type t = int
          
  $ cat << EOF | ./run.exe -stru -
  > type 'a my_list = 'a list
  > EOF
  Parsed: type 'a my_list = 'a list
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) pair = 'a * 'b
  > EOF
  Parsed: type ('a, 'b) pair = ('a * 'b)
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) arrow = 'a -> 'b
  > EOF
  Parsed: type ('a, 'b) arrow = ('a -> 'b)
          
#

# something more complex
  $ cat << EOF | ./run.exe -stru -
  > type 'a option =
  > | Some of 'a
  > | None
  > EOF
  Parsed: type 'a option =
            | Some of 'a
            | None
            
          
  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) arrows =
  >   | Normal of 'a -> 'b
  >   | Reversed of 'b -> 'a
  > EOF
  Parsed: type ('a, 'b) arrows =
            | Normal of ('a -> 'b)
            | Reversed of ('b -> 'a)
            
          

  $ cat << EOF | ./run.exe -stru -
  > type 'a list =
  > | Nil
  > | Cons of 'a * 'a list
  > EOF
  Parsed: type 'a list =
            | Nil
            | Cons of ('a * 'a list)
            
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) qwe =
  > | Asd of 'a -> ('a -> 'b) -> 'b
  > | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
  > EOF
  Parsed: type ('a, 'b) qwe =
            | Asd of ('a -> (('a -> 'b) -> 'b))
            | Zxc of (('a -> 'a) * ('a -> 'a) * 'a)
            
          
#

# "and" chains
  $ cat << EOF | ./run.exe -stru -
  > type a = int
  > and b = bool
  > and c = char
  > EOF
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
  > EOF
  Parsed: type 'a box =
            | Box of 'a
            
          and 't maybe =
            | Just of 't
            | Nothing
            
          and name = string
          

# invalid input
  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > EOF
  Error: : count_while1

  $ cat << EOF | ./run.exe -stru -
  > type foo = 123
  > EOF
  Error: : not a type param name

  $ cat << EOF | ./run.exe -stru -
  > type foo = a ->
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = a -> -> a
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = a *
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo = a * * a
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type a my_list = a list
  > EOF
  Error: : char '='

  $ cat << EOF | ./run.exe -stru -
  > type ''a my_list = ''a list
  > EOF
  Error: : not a type name

  $ cat << EOF | ./run.exe -stru -
  > type '_a my_list = '_a list
  > EOF
  Error: : not a type name

  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > | a
  > | b
  > EOF
  Error: : count_while1

  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > | A of
  > | B of
  > EOF
  Error: : end_of_input

  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > | A of A
  > | B of B
  > EOF
  Error: : end_of_input
#

# unsorted
  $ cat << EOF | ./run.exe -stru -
  > type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
  > EOF
  Parsed: type foo = (('a * 'b * 'c) -> (('d * 'e) -> 'f))
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (int -> int)
  > EOF
  Parsed: type foo = (int -> int)
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a -> b) * (c -> d)
  > EOF
  Parsed: type foo = ((a -> b) * (c -> d))
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a * b) * (c * d)
  > EOF
  Parsed: type foo = ((a * b) * (c * d))
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a -> b) -> (c -> d)
  > EOF
  Parsed: type foo = ((a -> b) -> (c -> d))
          
#
