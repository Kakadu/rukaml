tests for parser on structure item "type_definition"

# simple allias
  $ cat << EOF | ./REPL.exe -stru -
  > type a = int
  "type a = int"
  Parsed: type a =
            int
          
# type constructors
  $ cat << EOF | ./REPL.exe -stru -
  > type 'a my_list = 'a list
  "type 'a my_list = 'a list"
  Parsed: type 'a my_list =
            'a list
          

  $ cat << EOF | ./REPL.exe -stru -
  > type int_list = int list
  "type int_list = int list"
  Parsed: type int_list =
            int list
          

  $ cat << EOF | ./REPL.exe -stru -
  > type ('a, 'b) pair = 'a * 'b
  "type ('a, 'b) pair = 'a * 'b"
  Parsed: type ('a, 'b) pair =
            ('a, 'b)
          

  $ cat << EOF | ./REPL.exe -stru -
  > type ('t1, 't2, 't3, 't4, 't5, 't6) foo = ('t1 -> 't2 -> 't3) -> ('t4 -> 't5) -> 't6
  "type ('t1, 't2, 't3, 't4, 't5, 't6) foo = ('t1 -> 't2 -> 't3) -> ('t4 -> 't5) -> 't6"
  Parsed: type ('t1, 't2, 't3, 't4, 't5, 't6) foo =
            (('t1 -> ('t2 -> 't3)) -> (('t4 -> 't5) -> 't6))
          

# arrow
  $ cat << EOF | ./REPL.exe -stru -
  > type ('a, 'b) arrow = 'a -> 'b
  "type ('a, 'b) arrow = 'a -> 'b"
  Parsed: type ('a, 'b) arrow =
            ('a -> 'b)
          
  $ cat << EOF | ./REPL.exe -stru -
  > type ('a, 'b) arrow = ('a -> 'b) -> ('a -> 'b)
  "type ('a, 'b) arrow = ('a -> 'b) -> ('a -> 'b)"
  Parsed: type ('a, 'b) arrow =
            (('a -> 'b) -> ('a -> 'b))
          

# tuple
  $ cat << EOF | ./REPL.exe -stru -
  > type ('a, 'b) pair = 'a * 'b
  "type ('a, 'b) pair = 'a * 'b"
  Parsed: type ('a, 'b) pair =
            ('a, 'b)
          

  $ cat << EOF | ./REPL.exe -stru -
  > type ('a, 'b) pair_pair = ('a * 'b) * ('a * 'b)
  "type ('a, 'b) pair_pair = ('a * 'b) * ('a * 'b)"
  Parsed: type ('a, 'b) pair_pair =
            (('a, 'b), ('a, 'b))
          

# option
  $ cat << EOF | ./REPL.exe -stru -
  > type 'a option =
  > | Some of 'a
  > | None
  "type 'a option =\n| Some of 'a\n| None"
  Parsed: type 'a option =
            | Some of 'a
            | None
            
          

# list
  $ cat << EOF | ./REPL.exe -stru -
  > type 'a list =
  > | Nil
  > | Cons of 'a * 'a list
  "type 'a list =\n| Nil\n| Cons of 'a * 'a list"
  Parsed: type 'a list =
            | Nil
            | Cons of ('a, 'a list)
            
          

# something more complex
  $ cat << EOF | ./REPL.exe -stru -
  > type ('a, 'b) qwe =
  > | Asd of 'a -> ('a -> 'b) -> 'b
  > | Zxc of ('a -> 'a) * ('a -> 'a) * 'a
  "type ('a, 'b) qwe =\n| Asd of 'a -> ('a -> 'b) -> 'b\n| Zxc of ('a -> 'a) * ('a -> 'a) * 'a"
  Parsed: type ('a, 'b) qwe =
            | Asd of ('a -> (('a -> 'b) -> 'b))
            | Zxc of (('a -> 'a), ('a -> 'a), 'a)
            
          
# type definition chains with "and" keyword
  $ cat << EOF | ./REPL.exe -stru -
  > type a = int
  > and b = bool
  > and c = char
  "type a = int\nand b = bool\nand c = char"
  Parsed: type a =
            int
          and b =
            bool
          and c =
            char
          

  $ cat << EOF | ./REPL.exe -stru -
  > type 'a list =
  > | Nil
  > | Cons of 'a * 'a list
  > 
  > and 't option =
  > | Some of 't
  > | None
  > 
  > and name = string
  "type 'a list =\n| Nil\n| Cons of 'a * 'a list\n\nand 't option =\n| Some of 't\n| None\n\nand name = string"
  Parsed: type 'a list =
            | Nil
            | Cons of ('a, 'a list)
            
          and 't option =
            | Some of 't
            | None
            
          and name =
            string
          

# invalid input
  $ cat << EOF | ./REPL.exe -stru -
  > type foo =
  "type foo ="
  Error: : not enough input

  $ cat << EOF | ./REPL.exe -stru -
  > type foo = 123
  "type foo = 123"
  Error: : char '('

  $ cat << EOF | ./REPL.exe -stru -
  > type foo = a ->
  "type foo = a ->"
  Error: : end_of_input

  $ cat << EOF | ./REPL.exe -stru -
  > type foo = a -> -> a
  "type foo = a -> -> a"
  Error: : end_of_input

  $ cat << EOF | ./REPL.exe -stru -
  > type foo = a *
  "type foo = a *"
  Error: : end_of_input

  $ cat << EOF | ./REPL.exe -stru -
  > type foo = a * * a
  "type foo = a * * a"
  Error: : end_of_input

  $ cat << EOF | ./REPL.exe -stru -
  > type a my_list = a list
  "type a my_list = a list"
  Error: : char '='

  $ cat << EOF | ./REPL.exe -stru -
  > type ''a my_list = ''a list
  "type ''a my_list = ''a list"
  Error: : not a type name

  $ cat << EOF | ./REPL.exe -stru -
  > type '_a my_list = '_a list
  "type '_a my_list = '_a list"
  Error: : not a type name

  $ cat << EOF | ./REPL.exe -stru -
  > type foo =
  > | a
  > | b
  "type foo =\n| a\n| b"
  Error: : char '('

  $ cat << EOF | ./REPL.exe -stru -
  > type foo =
  > | A of
  > | B of
  "type foo =\n| A of\n| B of"
  Error: : end_of_input

  $ cat << EOF | ./REPL.exe -stru -
  > type foo =
  > | A of A
  > | B of B
  "type foo =\n| A of A\n| B of B"
  Error: : end_of_input

unsorted
  $ cat << EOF | ./REPL.exe -stru -
  > type foo = 'a * 'b * 'c -> 'd * 'e -> 'f
  "type foo = 'a * 'b * 'c -> 'd * 'e -> 'f"
  Parsed: type foo =
            (('a, 'b, 'c) -> (('d, 'e) -> 'f))
          

  $ cat << EOF | ./REPL.exe -stru -
  > type foo = (int -> int)
  "type foo = (int -> int)"
  Parsed: type foo =
            (int -> int)
          
  $ cat << EOF | ./REPL.exe -stru -
  > type foo = (a -> b) * (c -> d)
  "type foo = (a -> b) * (c -> d)"
  Parsed: type foo =
            ((a -> b), (c -> d))
          
  $ cat << EOF | ./REPL.exe -stru -
  > type foo = (a * b) * (c * d)
  "type foo = (a * b) * (c * d)"
  Parsed: type foo =
            ((a, b), (c, d))
          

  $ cat << EOF | ./REPL.exe -stru -
  > type foo = (a -> b) -> (c -> d)
  "type foo = (a -> b) -> (c -> d)"
  Parsed: type foo =
            ((a -> b) -> (c -> d))
          
