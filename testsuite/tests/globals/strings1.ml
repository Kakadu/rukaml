(*
   test
  (targets amd64)
  (run (stdout "(hello world!)"))
*)

let lp = '('
let rp = ')'
let hello = "hello"
let world = "world"
let bang = '!'
let main = printf "%c%s%s%c%c" lp hello world bang rp
