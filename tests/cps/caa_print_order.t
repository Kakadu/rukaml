  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./caa_print_order_cpsoff.exe
  rukaml_print_int 1
  rukaml_print_int 0

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./caa_print_order_cpson.exe
  rukaml_print_int 1
  rukaml_print_int 0

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./caa_print_order_callaron.exe
  rukaml_print_int 0
  rukaml_print_int 1

