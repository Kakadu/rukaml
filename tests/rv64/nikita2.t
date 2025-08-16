  $ ../../back/rv64/RV64_compiler.exe -o program.s --no-start -danf -stop-after anf nikita2.ml
  After ANF transformation.
  let revapply x k =
    k x 
  let fresh_2 x =
    0
  let fresh_1 z x =
    z fresh_2 
  let z k =
    0
  let main =
    let temp4 = fresh_1 z  in
      revapply 1 temp4
$ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
$ riscv64-linux-gnu-gcc-13 -c -g program.s -o nikita2.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back/rv64/rukaml_stdlib.o nikita2.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back/rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./nikita2.exe
