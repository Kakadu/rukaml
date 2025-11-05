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
  Parsed: ('a * 'b)

  $ cat << EOF | ./run.exe -core-type -
  > 'a -> 'b
  Parsed: ('a -> 'b)

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b) * ('b -> 'a)
  Parsed: (('a -> 'b) * ('b -> 'a))

  $ cat << EOF | ./run.exe -core-type -
  > int list
  Parsed: (int) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a) list
  Parsed: ('a) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a * 'b) list
  Parsed: (('a * 'b)) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b -> 'c) list
  Parsed: (('a -> ('b -> 'c))) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a, 'b) list
  Parsed: ('a, 'b) list

  $ cat << EOF | ./run.exe -core-type -
  > ('a -> 'b, 'c * 'd) list
  Parsed: (('a -> 'b), ('c * 'd)) list
#

# type declaration
  $ cat << EOF | ./run.exe -stru -
  > type t = int
  Parsed: type t =
            int
          
  $ cat << EOF | ./run.exe -stru -
  > type 'a my_list = 'a list
  Parsed: type 'a my_list =
            ('a) list
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) pair = 'a * 'b
  Parsed: type ('a, 'b) pair =
            ('a * 'b)
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) arrow = 'a -> 'b
  Parsed: type ('a, 'b) arrow =
            ('a -> 'b)
          
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
  > type 'a list =
  > | Nil
  > | Cons of 'a * 'a list
  Parsed: type 'a list =
            | Nil
            | Cons of ('a * ('a) list)
            
          

  $ cat << EOF | ./run.exe -stru -
  > type ('a, 'b) qwe =
  > | Asd of 'a -> ('a -> 'b) -> 'b
  > | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
  Parsed: type ('a, 'b) qwe =
            | Asd of ('a -> (('a -> 'b) -> 'b))
            | Zxc of (('a -> 'a) * ('a -> 'a) * 'a)
            
          
#

# "and" chains
  $ cat << EOF | ./run.exe -stru -
  > type a = int
  > and b = bool
  > and c = char
  Parsed: type a =
            int
          and b =
            bool
          and c =
            char
          

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
            
          and name =
            string
          

# invalid input
  $ cat << EOF | ./run.exe -stru -
  > type foo =
  Error: : count_while1

  $ cat << EOF | ./run.exe -stru -
  > type foo = 123
  Error: : not a type param name

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
  Error: : char '='

  $ cat << EOF | ./run.exe -stru -
  > type ''a my_list = ''a list
  Error: : not a type name

  $ cat << EOF | ./run.exe -stru -
  > type '_a my_list = '_a list
  Error: : not a type name

  $ cat << EOF | ./run.exe -stru -
  > type foo =
  > | a
  > | b
  Error: : count_while1

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
#

# unsorted
  $ cat << EOF | ./run.exe -stru -
  > type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
  Parsed: type foo =
            (('a * 'b * 'c) -> (('d * 'e) -> 'f))
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (int -> int)
  Parsed: type foo =
            (int -> int)
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a -> b) * (c -> d)
  Parsed: type foo =
            ((a -> b) * (c -> d))
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a * b) * (c * d)
  Parsed: type foo =
            ((a * b) * (c * d))
          

  $ cat << EOF | ./run.exe -stru -
  > type foo = (a -> b) -> (c -> d)
  Parsed: type foo =
            ((a -> b) -> (c -> d))
          
#
