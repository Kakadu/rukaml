  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./order1_cpsoff.exe
  rukaml_print_int 1000
  rukaml_print_int 100
  rukaml_print_int 1
  rukaml_print_int 10
  rukaml_print_int 100

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./order1_cpson.exe
  rukaml_print_int 1000
  rukaml_print_int 100
  rukaml_print_int 1
  rukaml_print_int 10
  rukaml_print_int 100

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./order1_callaron.exe
  rukaml_print_int 1000
  rukaml_print_int 100
  rukaml_print_int 1
  rukaml_print_int 10
  rukaml_print_int 100