  $ nasm -felf64 004fac_main.s -o main.o
  $ gcc 004fac.c -c -o 004fac.o
  $ ld -o fac main.o 004fac.o
  ld: warning: main.o: missing .note.GNU-stack section implies executable stack
  ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  $ chmod u+x fac
  $ ./fac
  [120]
  $ echo $?
  0
$ objdump -M intel -D 004fac.o
