  $ ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf nikita3.ml
  After ANF transformation.
  let fresh_2 print y =
    let z = print y  in
      0
  let fresh_1 print x =
    fresh_2 print x
  let main =
    fresh_1 print 1

$ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
$ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./nikita3.exe
  rukaml_print_int 1
