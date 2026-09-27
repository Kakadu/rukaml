
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'
  $ ls
$ chmod +w program.c
$ echo 'int main(){return;}' > program.c
$ cp program0.c program.c
$ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target parsetree




$ qemu-riscv64 $FLAGS ./compiler.exe -dc program0.c -o file.out --target rv64
