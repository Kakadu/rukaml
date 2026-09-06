
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'
  $ ls
  compiler.exe
  program0.c
$ chmod +w program.c
$ echo 'int main(){return;}' > program.c
$ cp program0.c program.c
$ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target parsetree




  $ qemu-riscv64 $FLAGS ./compiler.exe -dc program0.c -o file.out --target rv64
  
  
  GC statistics
  Total allocations: 527(words)
  Currently allocated: 527(words)
  Current bank: 0
  GC statistics
  Total allocations: 527(words)
  Currently allocated: 527(words)
  Current bank: 0
