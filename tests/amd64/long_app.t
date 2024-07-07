$ ls
  $ ./long_app.exe
  �
  rukaml_print_int 1111111111
  1
  rukaml_print_int 1
  :
  rukaml_print_int 10
  :
  rukaml_print_int 10
# Shadowing introduces a bug
  $ cat << EOF | ../../back_amd64/amd64_compiler.exe -o program.asm - #-vamd64
  > let wrap f = if 1 = 1 then f else f
  > let test3 a b c =
  >   let a1 = print a in
  >   let b = print b in
  >   let c = print c in
  >   0
  > let main =
  >   let temp2 = wrap test3 1 10 100 in
  >   0
  > EOF
  After ANF transformation.
  let wrap f =
    let temp1 = (1 = 1) in
      (if temp1
      then f
      else f)
  let test3 a b c =
    let a = print a  in
      let b = print b  in
        let c = print c  in
          0
  let main =
    let temp6 = wrap test3  in
      let temp7 = temp6 1  in
        let temp8 = temp7 10  in
          let temp2 = temp8 100  in
            0

; generated code for amd64
  $ cat program.asm  | grep -v 'section .note.GNU-stack' | nl -ba > /dev/null
  $ nasm -felf64 program.asm -o program.o
  $ gcc-12 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
  /usr/bin/ld: program.o: warning: relocation in read-only section `.text'
  /usr/bin/ld: warning: creating DT_TEXTREL in a PIE
  $ ./program.exe && echo $?
