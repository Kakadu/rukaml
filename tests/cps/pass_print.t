  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./pass_print_cpsoff.exe
  rukaml_print_int 21

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./pass_print_cpson.exe
  rukaml_print_int 21

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./pass_print_callaron.exe
  rukaml_print_int 21
