
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

  $ echo -n "void main(){}" > main.c
$ qemu-riscv64 $FLAGS ./compiler.exe -dc main.c -o file.out --target rv64
