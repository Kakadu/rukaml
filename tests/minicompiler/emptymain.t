
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

  $ echo -n "" > emptymain.c
  $ qemu-riscv64 $FLAGS ./compiler.exe -dc emptymain.c -o file.out --target rv64
  
  
  GC statistics
  Total allocations: 527(words)
  Currently allocated: 527(words)
  Current bank: 0
  GC statistics
  Total allocations: 527(words)
  Currently allocated: 527(words)
  Current bank: 0
