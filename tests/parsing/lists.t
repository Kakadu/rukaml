# tests list syntactic sugar behavior

# patterns
  $ cat << EOF | ./run.exe -pat -
  > []
  > EOF
  Parsed: []

  $ cat << EOF | ./run.exe -pat -
  > [ x; y; z ]
  > EOF
  Parsed: [ x; y; z ]

  $ cat << EOF | ./run.exe -pat -
  > [ (a, b); (c, d, e) ] 
  > EOF
  Parsed: [ a, b; c, d, e ]

  $ cat << EOF | ./run.exe -pat -
  > (x :: y, a :: b)
  > EOF
  Parsed: x :: y, a :: b

  $ cat << EOF | ./run.exe -pat -
  > x :: y :: z :: w
  > EOF
  Parsed: x :: y :: z :: w

  $ cat << EOF | ./run.exe -pat -
  > x :: [ a; b; c ]
  > EOF
  Parsed: [ x; a; b; c ]

  $ cat << EOF | ./run.exe -pat -
  > [ x ] :: [[ a ]; [ b ]; [ c ]]
  > EOF
  Parsed: [ [ x ]; [ a ]; [ b ]; [ c ] ]

  $ cat << EOF | ./run.exe -pat -
  > [ a ], [ b; c ], [ d; e; f ]
  > EOF
  Parsed: [ a ], [ b; c ], [ d; e; f ]

  $ cat << EOF | ./run.exe -pat -
  > ([], [[]], [[[]]])
  > EOF
  Parsed: [], [ [] ], [ [ [] ] ]

  $ cat << EOF | ./run.exe -pat -
  > (x, y) :: [ (x, y); (y, x) ]
  > EOF
  Parsed: [ x, y; x, y; y, x ]
#

# expressions
  $ cat << EOF | ./run.exe -e -
  > 1 + 2 :: [ 3; 4 ]
  > EOF
  Parsed: [ 1 + 2; 3; 4 ]

  $ cat << EOF | ./run.exe -e -
  > f x :: [ a; b ]
  > EOF
  Parsed: [ f x; a; b ]
#

# structures
  $ cat << EOF | ./run.exe -stru -
  > let rec length items =
  >   match items with
  >   | [] -> 0
  >   | _ :: xs -> 1 + length xs
  > EOF
  Parsed: let rec length items = (match items with
                                    | [] -> 0
                                    | _ :: xs -> 1 + length xs)

  $ cat << EOF | ./run.exe -stru -
  > let rec map f items =
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl 
  > EOF
  Parsed: let rec map f items = (match items with
                                   | [] -> []
                                   | hd :: tl -> f hd :: map f tl)

  $ cat << EOF | ./run.exe -stru -
  > let rec filter p items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> if p hd then hd :: filter p tl else filter p tl
  > EOF
  Parsed: let rec filter p items = (match items with
                                      | [] -> []
                                      | hd :: tl -> if p hd then hd :: filter p tl
                                                            else filter p tl)

  $ cat << EOF | ./run.exe -stru -
  > let rec filter p items acc = 
  >   match items with
  >   | [] -> acc
  >   | hd :: tl ->
  >     let acc2 = if p hd then hd :: acc else acc in
  >       filter p tl acc2
  > 
  > let filter p items = filter p items []
  > EOF
  Parsed: let rec filter p items acc = (match items with
                                          | [] -> acc
                                          | hd :: tl -> (let acc2 = if p hd 
                                                                    then hd :: acc
                                                                    else acc 
                                                         in filter p tl acc2))
  let filter p items = filter p items []

  $ cat << EOF | ./run.exe -stru -
  > let rec rev items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> hd :: rev items
  > EOF
  Parsed: let rec rev items = (match items with
                                 | [] -> []
                                 | hd :: tl -> hd :: rev items)

  $ cat << EOF | ./run.exe -stru -
  > let fold_left f init items =
  >   let rec helper items acc =
  >     match items with
  >       | [] -> acc
  >       | x :: xs -> helper xs (f x acc)
  >   in helper items init
  > EOF
  Parsed: let fold_left f init items = let rec helper items acc = (match items with
                                                                     | [] -> acc
                                                                     | x :: xs -> helper xs (f x acc)) 
                                       in helper items init
#
