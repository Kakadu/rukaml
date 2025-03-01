#cps off:
  $ cat ../rv64/order1.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o order1_cpsoff.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./order1_cpsoff.exe
  rukaml_print_int 1000
  rukaml_print_int 100
  rukaml_print_int 1
  rukaml_print_int 10
  rukaml_print_int 100


#cps on:
  $ cat ../rv64/order1.ml | ../../back_rv64/RV64_compiler.exe -o program.s -cps --no-start -
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o order1_cpson.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./order1_cpson.exe
  rukaml_print_int 1000
  rukaml_print_int 100
  rukaml_print_int 1
  rukaml_print_int 10
  rukaml_print_int 100
