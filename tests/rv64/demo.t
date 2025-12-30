$ ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf fac.ml
After ANF transformation.
let rec fac n =
let main =

$ cat program.s | grep -v 'section .note.GNU-stack'

$ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o # 2>&1 | head -n5
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fac.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu ./demo.exe
  rukaml_print_int 3
