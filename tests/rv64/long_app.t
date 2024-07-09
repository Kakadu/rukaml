Long application tests functions with many arguemnts.
In runtime function rukaml_applyN we need to support
arbitrary arity (not to hardcode arity).
  $  qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./long_app.exe
  rukaml_print_int 1111111111
  rukaml_print_int 1
  rukaml_print_int 10
  rukaml_print_int 100
