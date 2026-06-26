  $ ls
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'
  $ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target rv64
