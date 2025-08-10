  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./anon1_cpsoff.exe
  rukaml_print_int 255

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./anon1_cpson.exe
  rukaml_print_int 255

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./anon1_callaron.exe
  rukaml_print_int 255
