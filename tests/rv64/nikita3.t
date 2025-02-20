  $ cat nikita3.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let fresh_2 print y =
    let z = print y  in
      0
  let fresh_1 print x =
    let temp2 = fresh_2 print  in
      temp2 x 
  let main =
    let temp4 = fresh_1 print  in
      temp4 1 
  ANF: let fresh_2 print y =
         let z = print y  in
           0
       let fresh_1 print x =
         let temp2 = fresh_2 print  in
           temp2 x 
       let main =
         let temp4 = fresh_1 print  in
           temp4 1 
  Location argument "y" in [rbp+1]
  Location argument "print" in [rbp+0]
  Removing info about args [ print, y ]
  Location argument "x" in [rbp+1]
  Location argument "print" in [rbp+0]
  Removing info about args [ print, x ]
  Removing info about args [  ]
$ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
$ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./nikita3.exe
  rukaml_print_int 1
