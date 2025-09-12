  $ cat > test.ml << EOF
  > let large x = if x then print 0 else print 1
  > let main =
  >   let x = if (if (if false
  >                   then false else (let t42 = print 42 in true))
  >               then false else true)
  >           then false else true in
  >   large x
  > EOF
  $ cat test.ml | ./REPL.exe -
  Parsed: let large x = if x then print 0 else print 1
          let main = let x = if if if true then true else let t42 = print 42 
                                                          in true
                                then true else true
                             then true else true in large x
  After CCovv.
  let large: bool -> unit =
    fun x -> (if x then print 0 else print 1)
  let main: unit =
    let x : bool = (if (if (if true then true else let t42 : unit = print 42 in
    true) then true else true) then true else true) in
    large x
  After ANF transformation.
  let large x =
    (if x
    then print 0 
    else print 1 )
  let main =
    let temp4 = (if true
                then true
                else let t42 = print 42  in
                       true) in
      let temp5 = (if temp4
                  then true
                  else true) in
        let x = (if temp5
                then true
                else true) in
          large x 
  $ ../../back_amd64/amd64_compiler.exe test.ml -o test.s
  $ nasm -felf64 test.s -o program.o
  $ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  $ ./program.exe
  rukaml_print_int 0

But OCaml prints 42 too
  $ echo 'let print n =Printf.printf "%d\n" n;;' > a.ml
  $ cat test.ml >> a.ml
  $ ocaml -w -26 a.ml
  42
  0
