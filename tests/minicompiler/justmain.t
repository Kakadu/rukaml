
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

  $ echo -n "void main(){}" > main.c
  $ qemu-riscv64 $FLAGS ./compiler.exe -dc main.c -o file.out --target rv64
  void main(){}
  
  GC statistics
  Total allocations: 710(words)
  Currently allocated: 710(words)
  Current bank: 0
  parsing failed: can not parse many program items (failed at ~2 line)
  GC statistics
  Total allocations: 727(words)
  Currently allocated: 727(words)
  Current bank: 0


