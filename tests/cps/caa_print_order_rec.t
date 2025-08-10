  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./caa_print_order_rec_cpsoff.exe
  rukaml_print_int 2
  rukaml_print_int 3
  rukaml_print_int 4

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./caa_print_order_rec_cpson.exe
  rukaml_print_int 2
  rukaml_print_int 3
  rukaml_print_int 4

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./caa_print_order_rec_callaron.exe
  rukaml_print_int 2
  rukaml_print_int 3
  rukaml_print_int 4

