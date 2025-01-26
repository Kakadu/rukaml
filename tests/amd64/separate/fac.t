$ ls
$ ./compiler.exe -o fac.asm -vamd64 -c fac.ml
; generated code for amd64
$ cat fac.asm | grep -v 'section .note.GNU-stack' | nl -ba

$ ./compiler.exe -o fac_main.asm -vamd64 -c fac.rkmi fac_main.ml
; generated code for amd64
$ cat fac_main.asm | grep -v 'section .note.GNU-stack' | nl -ba

$ ls
$ nasm -felf64 fac_main.asm -o fac.o
$ nasm -felf64 fac_main.asm -o program.o
$ gcc-13 -g -o program.exe fac.o program.o  stdlib.o && ./program.exe
$ ld -g -o program.exe stdlib.o program.o && ./program.exe
rukaml_print_int 24
  $ ./fac_main.exe
  rukaml_print_int 15
  rukaml_print_int 24
