  $ cat fib2.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let return x =
    x
  let fresh_2 t2 k1 t3 =
    let temp1 = (t2 + t3) in
      k1 temp1 
  let fresh_1 n k1 fib t2 =
    let temp3 = (n - 2) in
      let temp4 = fib temp3  in
        let temp5 = fresh_2 t2  in
          let temp6 = temp5 k1  in
            temp4 temp6 
  let fresh_3 print t4 =
    let g = print t4  in
      return 0 
  let fib n k1 =
    let temp10 = (n < 2) in
      (if temp10
      then k1 n 
      else let temp12 = (n - 1) in
             let temp13 = fib temp12  in
               let temp14 = fresh_1 n  in
                 let temp15 = temp14 k1  in
                   let temp16 = temp15 fib  in
                     temp13 temp16 )
  let main =
    let temp19 = fib 6  in
      let temp20 = fresh_3 print  in
        temp19 temp20 
  ANF: let return x =
         x
       let fresh_2 t2 k1 t3 =
         let temp1 = (t2 + t3) in
           k1 temp1 
       let fresh_1 n k1 fib t2 =
         let temp3 = (n - 2) in
           let temp4 = fib temp3  in
             let temp5 = fresh_2 t2  in
               let temp6 = temp5 k1  in
                 temp4 temp6 
       let fresh_3 print t4 =
         let g = print t4  in
           return 0 
       let fib n k1 =
         let temp10 = (n < 2) in
           (if temp10
           then k1 n 
           else let temp12 = (n - 1) in
                  let temp13 = fib temp12  in
                    let temp14 = fresh_1 n  in
                      let temp15 = temp14 k1  in
                        let temp16 = temp15 fib  in
                          temp13 temp16 )
       let main =
         let temp19 = fib 6  in
           let temp20 = fresh_3 print  in
             temp19 temp20 
  Location argument "x" in [rbp+0]
  Removing info about args [ x ]
  Location argument "t3" in [rbp+2]
  Location argument "k1" in [rbp+1]
  Location argument "t2" in [rbp+0]
  Removing info about args [ t2, k1, t3 ]
  Location argument "t2" in [rbp+3]
  Location argument "fib" in [rbp+2]
  Location argument "k1" in [rbp+1]
  Location argument "n" in [rbp+0]
  Removing info about args [ n, k1, fib, t2 ]
  Location argument "t4" in [rbp+1]
  Location argument "print" in [rbp+0]
  Removing info about args [ print, t4 ]
  Location argument "k1" in [rbp+1]
  Location argument "n" in [rbp+0]
  Removing info about args [ n, k1 ]
  Removing info about args [  ]
$ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
$ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib2.exe
  Segmentation fault
  [139]
