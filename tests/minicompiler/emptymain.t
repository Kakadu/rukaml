
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

  $ echo -n "" > emptymain.c
$ qemu-riscv64 $FLAGS ./compiler.exe -dc emptymain.c -o file.out --target rv64
