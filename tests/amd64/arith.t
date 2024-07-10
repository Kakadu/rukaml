We currentlyu doesn't support toplevel constants, so the following crashes
$ cat > demo.ml << EOF
> let c = 2
> let sq y = y * c
> let main = sq 7
> EOF

$ cat demo.ml | ../../back_amd64/amd64_compiler.exe -o program.asm -

; generated code for amd64
$ cat program.asm  | grep -v 'section .note.GNU-stack' | nl -ba
$ nasm -felf64 program.asm -o program.o
$ gcc-13 program.o ../../back_amd64/rukaml_stdlib.o -o program.exe
$ ./program.exe && echo $?
