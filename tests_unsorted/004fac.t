  $ nasm -felf64 004fac_main.s -o main.o
  $ gcc 004fac.c -c -o 004fac.o
  $ ld -o fac main.o 004fac.o
  $ chmod u+x fac
  $ ./fac
  [120]
  $ echo $?
  0
$ objdump -M intel -D 004fac.o
