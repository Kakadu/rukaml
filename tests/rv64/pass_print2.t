  $ cat pass_print2.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
