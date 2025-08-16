$ cat << EOF | ../../back/amd64/amd64_compiler.exe -o program.asm -vamd64 -
> let apply_zero f = f 21
> let main =
>   let x = apply_zero print in
>   0
> EOF
> let apply_two f = let b = f 42 in f 42
>   let y = apply_two print in


; generated code for amd64
$ cat program.asm  | grep -v 'section .note.GNU-stack' | nl -ba


$ nasm -felf64 program.asm -o program.o && ld -o program.exe program.o && chmod u+x program.exe && ./program.exe
$ nasm -felf64 program.asm -o program.o
$ gcc-13 program.o ../../back/amd64/rukaml_stdlib.o -o program.exe
  $ ./pass_print.exe
  rukaml_print_int 21
  rukaml_print_int 22
