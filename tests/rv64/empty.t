  $  ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf -stop-after anf empty.ml
  After ANF transformation.
  let main =
    0

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./empty.exe; echo $?
  0
