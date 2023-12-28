  $ cat fac.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let fac n =
    let temp1 = (n = 1) in
      (if temp1
      then 1
      else let p = (n - 1) in
             let p2 = fac p  in
               (n * p2))
  let main =
    let n = fac 4  in
      0
  ANF: let fac n =
         let temp1 = (n = 1) in
           (if temp1
           then 1
           else let p = (n - 1) in
                  let p2 = fac p  in
                    (n * p2))
       let main =
         let n = fac 4  in
           0
  Location argument "n" in [rbp+2]
  Removing info about args [ n ]
  Removing info about args [  ]
 
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
  $ ls 

  $ riscv64-linux-gnu-gcc program.s rukaml_stdlib.o -o program.o 2>&1 | head -n5
$ riscv64-linux-gnu-gcc -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  