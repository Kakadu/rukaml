#cps off:
  $ cat ../rv64/fac.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -
  After ANF transformation.
  let rec fac n =
    let temp = (n = 1) in
      (if temp
      then 1
      else let p = (n - 1) in
             let p2 = fac p  in
               (n * p2))
  let main =
    let f = fac 4  in
      let g = print f  in
        0

  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fac_cpsoff.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fac_cpsoff.exe
  rukaml_print_int 24


#cps on
  $ cat ../rv64/fac.ml | ../../back_rv64/RV64_compiler.exe -o program.s -cps --no-start -
  After ANF transformation.
  let fresh_1 n k1 t2 =
    let temp4 = (n * t2) in
      k1 temp4 
  let fresh_2 print t3 =
    let g = print t3  in
      0
  let fac n k1 =
    let temp7 = (n = 1) in
      (if temp7
      then k1 1 
      else let temp9 = (n - 1) in
             let temp10 = fac temp9  in
               let temp11 = fresh_1 n  in
                 let temp12 = temp11 k1  in
                   temp10 temp12 )
  let main =
    let temp15 = fac 4  in
      let temp16 = fresh_2 print  in
        temp15 temp16 

  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fac_cpson.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fac_cpson.exe
  rukaml_print_int 24
