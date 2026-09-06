
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

$ cat ../../run_rv32


  $ chmod +w program0.c
  $ echo "int main(){}" > program0.c

  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program0.c -o file.out --target parsetree
  int main(){}
  
  
  GC statistics
  Total allocations: 809(words)
  Currently allocated: 809(words)
  Current bank: 0
  parsing failed: can not parse many program items (failed at ~3 line)
  GC statistics
  Total allocations: 826(words)
  Currently allocated: 826(words)
  Current bank: 0
