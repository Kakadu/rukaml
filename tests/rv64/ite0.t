  $ ../../back/rv64/RV64_compiler.exe -o program.s --no-start -danf -stop-after anf ite0.ml
  After ANF transformation.
  let foo n =
    (if (n < 2)
    then 1
    else 2)
  let main =
    let x = foo 6  in
      let w = print x  in
        0

  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./ite0.exe
  rukaml_print_int 2
