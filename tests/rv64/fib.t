  $ cat fib.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
  
  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o  
$ riscv64-linux-gnu-gcc -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib.exe 
