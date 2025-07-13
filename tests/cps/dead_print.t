  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./dead_print_cpsoff.exe
  rukaml_print_int 1
  rukaml_print_int 1

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./dead_print_cpson.exe
  rukaml_print_int 1
  rukaml_print_int 1

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./dead_print_callaron.exe
