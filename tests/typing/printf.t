  $ run () { ../../driver/driver.exe $1 --target typedtree -o a.ml && cat a.ml; }

  $ run << EOF
  > let s = sprintf "%s" "hello world"
  > EOF
  let s: string =
    (sprintf "%s") "hello world"


  $ run << EOF
  > let s = sprintf "%d %b %s"
  > EOF
  let s: int -> bool -> string -> string =
    sprintf "%d %b %s"


  $ run << EOF
  > let s = sprintf "%d %b %s" 1
  > EOF
  let s: bool -> string -> string =
    (sprintf "%d %b %s") 1


  $ run << EOF
  > let s = sprintf "%d %b %s" 1 true
  > EOF
  let s: string -> string =
    ((sprintf "%d %b %s") 1) true


  $ run << EOF
  > let s = sprintf "%d %b %s" 1 true "one"
  > EOF
  let s: string =
    (((sprintf "%d %b %s") 1) true) "one"


  $ run << EOF
  > let pp_string oc s = fprintf oc "%s" s
  > let s = sprintf "%a" pp_string "123"
  > EOF
  infer error: unification failed on unit and out_channel
  [1]
