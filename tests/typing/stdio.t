  $ run () { ../../driver/driver.exe $1 --target typedtree --no-cconv -o a.ml && cat a.ml; }

  $ run << EOF
  > let u = output_string stdout "hello world"
  > EOF
  let u: unit =
    (output_string stdout) "hello world"

  $ run << EOF
  > let u = printf "hello world"
  > EOF
  let u: unit =
    printf "hello world"

  $ run << EOF
  > let u = printf "%s" "hello world"
  > EOF
  let u: unit =
    (printf "%s") "hello world"

  $ run << EOF
  > let u = fprintf stdout "%s" "hello world"
  > EOF
  let u: unit =
    ((fprintf stdout) "%s") "hello world"

  $ run << EOF
  > let pp_print_string oc s = fprintf oc "%s" s
  > let u = printf "%a" pp_print_string "hello world"
  > EOF
  let pp_print_string: out_channel -> string -> unit =
    fun oc s -> ((fprintf oc) "%s") s
  let u: unit =
    ((printf "%a") pp_print_string) "hello world"

example:
  $ run << EOF
  > let main =
  >   let in_channel = open_in "in.txt" in
  >   let line = input_line in_channel in
  >   let () = close_in in_channel in
  >   let out_channel = open_out "out.txt" in
  >   let () = output_string out_channel line in
  >   close_out out_channel
  > EOF
  let main: unit =
    let in_channel : in_channel = open_in "in.txt" in
    let line : string = input_line in_channel in
    let () : unit = close_in in_channel in
    let out_channel : out_channel = open_out "out.txt" in
    let () : unit = (output_string out_channel) line in
    close_out out_channel

  $ run << EOF
  > let printf = fprintf stdout
  > EOF
  let printf: ('_weak1, out_channel, unit) format3 -> '_weak1 =
    fprintf stdout

  $ run << EOF
  > let f = fprintf
  > let g oc = fprintf oc
  > let h oc fmt = fprintf oc fmt
  > EOF
  let f: out_channel -> ('_1, out_channel, unit) format3 -> '_1 =
    fprintf
  let g: out_channel -> ('_2, out_channel, unit) format3 -> '_2 =
    fun oc -> fprintf oc
  let h: out_channel -> ('_5, out_channel, unit) format3 -> '_5 =
    fun oc fmt -> (fprintf oc) fmt

  $ run << EOF
  > let f = printf
  > let g = printf "%s"
  > let h = printf "%s -> %s"
  > let i = printf "%s -> %s" "one"
  > let j = printf "%s -> %s" "one" "two"
  > EOF
  let f: ('_1, out_channel, unit) format3 -> '_1 =
    printf
  let g: string -> unit =
    printf "%s"
  let h: string -> string -> unit =
    printf "%s -> %s"
  let i: string -> unit =
    (printf "%s -> %s") "one"
  let j: unit =
    ((printf "%s -> %s") "one") "two"

# let polymorphism with format3 type
  $ run << EOF
  > let f fmt = printf fmt "hello_world"
  > EOF
  let f: (string -> '_4, out_channel, unit) format3 -> '_4 =
    fun fmt -> (printf fmt) "hello_world"

  $ run << EOF
  > let f fmt = printf fmt 1 true "one"
  > EOF
  let f: (int -> bool -> string -> '_6, out_channel, unit) format3 -> '_6 =
    fun fmt -> (((printf fmt) 1) true) "one"

  $ run << EOF
  > let f oc fmt = fprintf oc fmt 1
  > EOF
  let f: out_channel -> (int -> '_6, out_channel, unit) format3 -> '_6 =
    fun oc fmt -> ((fprintf oc) fmt) 1

  $ run << EOF
  > let pp_int oc n = fprintf oc "%d" n
  > let pp_char oc c = fprintf oc "%c" c
  > let pp_bool oc b = fprintf oc "%b" b
  > let pp_string oc s = fprintf oc "%s" s
  > 
  > let u = fprintf stdout "%a" pp_int 1
  > let u = fprintf stdout "%a" pp_char '1'
  > let u = fprintf stdout "%a" pp_bool true
  > let u = fprintf stdout "%a" pp_string "one"
  > EOF
  let pp_int: out_channel -> int -> unit =
    fun oc n -> ((fprintf oc) "%d") n
  let pp_char: out_channel -> char -> unit =
    fun oc c -> ((fprintf oc) "%c") c
  let pp_bool: out_channel -> bool -> unit =
    fun oc b -> ((fprintf oc) "%b") b
  let pp_string: out_channel -> string -> unit =
    fun oc s -> ((fprintf oc) "%s") s
  let u: unit =
    (((fprintf stdout) "%a") pp_int) 1
  let u: unit =
    (((fprintf stdout) "%a") pp_char) '1'
  let u: unit =
    (((fprintf stdout) "%a") pp_bool) true
  let u: unit =
    (((fprintf stdout) "%a") pp_string) "one"

# multiple specifiers
  $ run << EOF
  > let a = fprintf stdout "%a"
  > let aa = fprintf stdout "%a %a"
  > let aaa = fprintf stdout "%a %a %a"
  > EOF
  let a: (out_channel -> '_5 -> unit) -> '_5 -> unit =
    (fprintf stdout) "%a"
  let aa: (out_channel -> '_6 -> unit) -> '_6 -> (out_channel -> '_5 -> unit) -> '_5 -> unit =
    (fprintf stdout) "%a %a"
  let aaa: (out_channel -> '_7 -> unit) -> '_7 -> (out_channel -> '_6 -> unit) -> '_6 -> (out_channel -> '_5 -> unit) -> '_5 -> unit =
    (fprintf stdout) "%a %a %a"

  $ run << EOF
  > let pp_int oc n = fprintf oc "%d" n
  > let pp_string oc s = fprintf oc "%s" s
  > let u = fprintf stdout "%a %a %a %a %a" pp_int 1 pp_string "2" pp_int 3 pp_int 4 pp_string "5"
  > EOF
  let pp_int: out_channel -> int -> unit =
    fun oc n -> ((fprintf oc) "%d") n
  let pp_string: out_channel -> string -> unit =
    fun oc s -> ((fprintf oc) "%s") s
  let u: unit =
    (((((((((((fprintf stdout) "%a %a %a %a %a") pp_int) 1) pp_string) "2") pp_int) 3) pp_int) 4) pp_string) "5"

  $ run << EOF
  > let pp_str oc s = fprintf oc "%s" s
  > let u = printf "%a %s" pp_str "hello" "world"
  > EOF
  let pp_str: out_channel -> string -> unit =
    fun oc s -> ((fprintf oc) "%s") s
  let u: unit =
    (((printf "%a %s") pp_str) "hello") "world"

  $ run << EOF
  > let u = printf "%d %s %b" 1 "one" true
  > EOF
  let u: unit =
    (((printf "%d %s %b") 1) "one") true

# invalid input

should fail
  $ run << EOF
  > let t = printf "%s" 42
  > EOF
  infer error: unification failed on string and int
  [1]

should fail
  $ run << EOF
  > let t = printf "%d" 1 2
  > EOF
  infer error: unification failed on unit and (int -> '_6)
  [1]

should fail
  $ run << EOF  
  > let pp_int oc n = fprintf oc "%d" n
  > let u = fprintf stdout "%a" pp_int '1'
  > EOF
  infer error: unification failed on int and char
  [1]

# weird cases

should fail
  $ run << EOF
  > let fmt = "%s"
  > let u = printf fmt "hello world"
  > EOF
  infer error: unification failed on '_1, out_channel, unit format3 and string
  [1]

should fail
  $ run << EOF
  > let u = printf (let fmt = "%s" in fmt) "hello world"
  > EOF
  infer error: unification failed on '_1, out_channel, unit format3 and string
  [1]

should pass
  $ run << EOF
  > let u = printf (let () = () in "%s") "hello world"
  > EOF
  let u: unit =
    (printf let () : unit = () in
    "%s") "hello world"

should pass
  $ run << EOF
  > let pp_string oc parens s =
  >   fprintf oc (if parens then "(%s)" else "%s") s
  > EOF
  let pp_string: out_channel -> bool -> string -> unit =
    fun oc parens s -> ((fprintf oc) (if parens then "(%s)" else "%s")) s

should fail
  $ run << EOF
  > let main = printf (if true then "%d" else "%s")
  > EOF
  infer error: unification failed on int and string
  [1]

should pass
  $ run << EOF
  > let pp_string oc parens s =
  >   fprintf oc
  >     (match parens with
  >      | true -> "(%s)"
  >      | false -> "%s")
  >     s
  > EOF
  let pp_string: out_channel -> bool -> string -> unit =
    fun oc parens s -> ((fprintf oc) (match parens with
                                        | true -> "(%s)"
                                        | false -> "%s")) s

should fail
  $ run << EOF
  > let main =
  >   printf
  >     (match true with
  >      | true -> "%d"
  >      | false -> "%s")
  > EOF
  infer error: unification failed on string and int
  [1]
