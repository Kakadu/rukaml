# tests list syntactic sugar behavior

# patterns
  $ cat << EOF | ./run.exe -pat -
  > []
  Parsed: []

  $ cat << EOF | ./run.exe -pat -
  > [ x; y; z ]
  Parsed: [ x; y; z ]

  $ cat << EOF | ./run.exe -pat -
  > [ (a, b); (c, d, e) ] 
  Parsed: [ (a, b); (c, d, e) ]

  $ cat << EOF | ./run.exe -pat -
  > (x :: y, a :: b)
  Parsed: (x :: y, a :: b)

  $ cat << EOF | ./run.exe -pat -
  > x :: y :: z :: w
  Parsed: x :: y :: z :: w

  $ cat << EOF | ./run.exe -pat -
  > x :: [ a; b; c ]
  Parsed: [ x; a; b; c ]

  $ cat << EOF | ./run.exe -pat -
  > [ x ] :: [[ a ]; [ b ]; [ c ]]
  Parsed: [ [ x ]; [ a ]; [ b ]; [ c ] ]

  $ cat << EOF | ./run.exe -pat -
  > [ a ], [ b; c ], [ d; e; f ]
  Parsed: ([ a ], [ b; c ], [ d; e; f ])

  $ cat << EOF | ./run.exe -pat -
  > ([], [[]], [[[]]])
  Parsed: ([], [ [] ], [ [ [] ] ])

  $ cat << EOF | ./run.exe -pat -
  > (x, y) :: [ (x, y); (y, x) ]
  Parsed: [ (x, y); (x, y); (y, x) ]
#

# expressions
  $ cat << EOF | ./run.exe -e -
  > 1 + 2 :: [ 3; 4 ]
  Parsed: [ 1 + 2; 3; 4 ]

  $ cat << EOF | ./run.exe -e -
  > f x :: [ a; b ]
  Parsed: [ f x; a; b ]
#

# structures
  $ cat << EOF | ./run.exe -stru -
  > let rec length items =
  >   match items with
  >   | [] -> 0
  >   | _ :: xs -> 1 + length xs
  Parsed: let rec length items = match items with
                                   | [] -> 0
                                   | _ :: xs -> 1 + (length xs)
                                   

  $ cat << EOF | ./run.exe -stru -
  > let rec map f items =
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> f hd :: map f tl 
  Parsed: let rec map f items = match items with
                                  | [] -> []
                                  | hd :: tl -> f hd :: (map f tl)
                                  

  $ cat << EOF | ./run.exe -stru -
  > let rec filter p items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> if p hd then hd :: filter p tl else filter p tl
  Parsed: let rec filter p items = match items with
                                     | [] -> []
                                     | hd :: tl -> if p hd then hd :: (
                                                           filter p tl) 
                                                   else filter p tl
                                     

  $ cat << EOF | ./run.exe -stru -
  > let rec filter p items acc = 
  >   match items with
  >   | [] -> acc
  >   | hd :: tl ->
  >     let acc2 = if p hd then hd :: acc else acc in
  >       filter p tl acc2
  > 
  > let filter p items = filter p items []
  Parsed: let rec filter p items acc = match items with
                                         | [] -> acc
                                         | hd :: tl -> let acc2 = if p hd 
                                                                  then hd :: acc
                                                                  else acc 
                                                       in filter p tl acc2
                                         
          let filter p items = filter p items []

  $ cat << EOF | ./run.exe -stru -
  > let rec rev items = 
  >   match items with
  >   | [] -> []
  >   | hd :: tl -> hd :: rev items
  Parsed: let rec rev items = match items with
                                | [] -> []
                                | hd :: tl -> hd :: (rev items)
                                

  $ cat << EOF | ./run.exe -stru -
  > let fold_left f init items =
  >   let rec helper items acc =
  >     match items with
  >       | [] -> acc
  >       | x :: xs -> helper xs (f x acc)
  >   in helper items init
  Parsed: let fold_left f init items = let rec helper items acc = match items with
                                                                    | [] -> acc
                                                                    | x :: 
                                                                    xs -> 
                                                                    helper 
                                                                    xs 
                                                                    (f x acc)
                                                                     in 
                                       helper items init
#
