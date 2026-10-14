
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

  $ echo -n "" > emptymain.c
  $ qemu-riscv64 $FLAGS ./compiler.exe -dc emptymain.c -o file.out --target rv64
  
  
  GC statistics
  Total allocations: 1024(words)
  Currently allocated: 1024(words)
  Current bank: 0
  GC statistics
  Total allocations: 1048(words)
  Currently allocated: 1048(words)
  Current bank: 0
