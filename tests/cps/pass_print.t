#cps off:
  $ cat ../rv64/pass_print.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o pass_print_cpsoff.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./pass_print_cpsoff.exe
  rukaml_print_int 21


#cps on:
  $ cat ../rv64/pass_print.ml | ../../back_rv64/RV64_compiler.exe -o program.s -cps --no-start -
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o pass_print_cpson.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./pass_print_cpson.exe
  rukaml_print_int 21
