set history save
set architecture riscv:rv64
set sysroot /usr/riscv64-linux-gnu
target remote :1234
layout asm
layout reg
b main
c
x/8xg $sp