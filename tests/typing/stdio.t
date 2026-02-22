  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ run << EOF
  > let main =
  >   output_string stdout "hello world"
  > EOF
  let main: unit =
    (output_string stdout) "hello world"

opens "in.txt" for read as in_channel;
reads line from in_channel;
closes in_channel;
opens "out.txt" for write as out_channel;
writes line to out_channel;
closes out_channel.
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
  let printf: ('_weak1, out_channel, unit) format3 -> '_weak1 =
    fprintf stdout

  $ run << EOF
  > let f = fprintf
  > let g oc = fprintf oc
  > let h oc fmt = fprintf oc fmt
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
  let f: (string -> '_4, out_channel, unit) format3 -> '_4 =
    fun fmt -> (printf fmt) "hello_world"

  $ run << EOF
  > let f fmt = printf fmt 1 true "one"
  let f: (int -> bool -> string -> '_6, out_channel, unit) format3 -> '_6 =
    fun fmt -> (((printf fmt) 1) true) "one"

  $ run << EOF
  > let f oc fmt = fprintf oc fmt 1
  let f: out_channel -> (int -> '_6, out_channel, unit) format3 -> '_6 =
    fun oc fmt -> ((fprintf oc) fmt) 1

# edge cases

should fail
  $ run << EOF
  > let s = "%s"
  > let u = printf s "hello world"
  infer error: unification failed on '_1, out_channel, unit format3 and string
  [1]

should pass
  $ run << EOF
  > let u = printf "%s" "hello world"
  let u: unit =
    (printf "%s") "hello world"

# invalid input

  $ run << EOF
  > let t = printf "%s" 42
  infer error: unification failed on string and int
  [1]

  $ run << EOF
  > let t = printf "%d" 1 2
  infer error: unification failed on unit and (int -> '_6)
  [1]
