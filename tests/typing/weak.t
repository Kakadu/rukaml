  $ cat << EOF | ./REPL.exe -stru -
  > let f = [||]
  Parsed.
  let f = [||]
  let f: '_1 array =
    [||]

  $ cat << EOF | ./REPL.exe -stru -
  > let f = [|[||]|]
  Parsed.
  let f = [|[||]|]
  let f: '_weak1 array array =
    [|[||]|]

  $ cat << EOF | ./REPL.exe -stru -
  > let pair x = (x, x)
  > let g = [| pair |]
  > let h x = [| pair x |]
  Parsed.
  let pair x = (x, x)
  let g = [|pair|]
  let h x = [|pair x|]
  let pair: '_1 -> '_1 * '_1 =
    fun x -> (x, x)
  let g: '_weak1 -> '_weak1 * '_weak1 array =
    [|pair|]
  let h: '_1 -> '_1 * '_1 array =
    fun x -> [|pair x|]

  $ cat << EOF | ./REPL.exe -stru -
  > let f x = [| x |]
  Parsed.
  let f x = [|x|]
  let f: '_1 -> '_1 array =
    fun x -> [|x|]

Without eta-expansion
  $ cat << EOF | ./REPL.exe -stru -
  > let pair x y = (x, y)
  > let g = pair 1
  > let temp = g 2
  Parsed.
  let pair x y = (x, y)
  let g = pair 1
  let temp = g 2
  let pair: '_1 -> '_2 -> '_1 * '_2 =
    fun x y -> (x, y)
  let g: '_weak1 -> int * '_weak1 =
    pair 1
  let temp: int * int =
    g 2


With eta-expansion but we not use the passed argument
  $ cat << EOF | ./REPL.exe -stru -
  > let pair x y = (x, y)
  > let g x = pair 1
  > let temp = g 2
  Parsed.
  let pair x y = (x, y)
  let g x = pair 1
  let temp = g 2
  let pair: '_1 -> '_2 -> '_1 * '_2 =
    fun x y -> (x, y)
  let g: '_1 -> '_4 -> int * '_4 =
    fun x -> pair 1
  let temp: '_weak1 -> int * '_weak1 =
    g 2

With eta-expansion
  $ cat << EOF | ./REPL.exe -stru -
  > let pair x y = (x, y)
  > let g x = pair 1 x
  Parsed.
  let pair x y = (x, y)
  let g x = pair 1 x
  let pair: '_1 -> '_2 -> '_1 * '_2 =
    fun x y -> (x, y)
  let g: '_1 -> int * '_1 =
    fun x -> (pair 1) x

  $ cat << EOF | ./REPL.exe -stru -
  > let tuple6 x y z a b c = (x,y,z,a,b,c)
  > let g = tuple6 1
  > let f x = tuple6 x
  Parsed.
  let tuple6 x y z a b c = (x, y, z, a, b, c)
  let g = tuple6 1
  let f x = tuple6 x
  let tuple6: '_1 -> '_2 -> '_3 -> '_4 -> '_5 -> '_6 -> '_1 * '_2 * '_3 * '_4 * '_5 * '_6 =
    fun x y z a b c -> (x, y, z, a, b, c)
  let g: '_weak5 -> '_weak4 -> '_weak3 -> '_weak2 -> '_weak1 -> int * '_weak5 * '_weak4 * '_weak3 * '_weak2 * '_weak1 =
    tuple6 1
  let f: '_1 -> '_8 -> '_9 -> '_10 -> '_11 -> '_12 -> '_1 * '_8 * '_9 * '_10 * '_11 * '_12 =
    fun x -> tuple6 x

Weak type elimination
  $ cat << EOF | ./REPL.exe -stru -
  > let f x y = x
  > let g = f 1
  > let h = g 2
  > let l = g
  Parsed.
  let f x y = x
  let g = f 1
  let h = g 2
  let l = g
  let f: '_1 -> '_2 -> '_1 =
    fun x y -> x
  let g: '_weak1 -> int =
    f 1
  let h: int =
    g 2
  let l: int -> int =
    g
