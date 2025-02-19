  $ cat << EOF | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  > let main =
  >   let ret x k =  k x in
  >   let z k = 0 in
  >   ret 1 (fun x -> z (fun x -> 0))
  > EOF
  After ANF transformation.
  let fresh_2 x =
    0
  let fresh_1 z x =
    z fresh_2 
  let z k =
    0
  let ret x k =
    k x 
  let main =
    let temp3 = ret 1  in
      let temp4 = fresh_1 z  in
        temp3 temp4 
  ANF: let fresh_2 x =
         0
       let fresh_1 z x =
         z fresh_2 
       let z k =
         0
       let ret x k =
         k x 
       let main =
         let temp3 = ret 1  in
           let temp4 = fresh_1 z  in
             temp3 temp4 
  Location argument "x" in [rbp+0]
  Removing info about args [ x ]
  Location argument "x" in [rbp+1]
  Location argument "z" in [rbp+0]
  Removing info about args [ z, x ]
  Location argument "k" in [rbp+0]
  Removing info about args [ k ]
  Location argument "k" in [rbp+1]
  Location argument "x" in [rbp+0]
  Removing info about args [ x, k ]
  Removing info about args [  ]
$ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
