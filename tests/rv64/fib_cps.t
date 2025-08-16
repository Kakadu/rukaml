  $ ../../back/rv64/RV64_compiler.exe -o program.s --no-start -danf -stop-after anf fib_cps.ml
  After ANF transformation.
  let id x =
    x
  let fresh_2 p1 k p2 =
    let temp1 = (p1 + p2) in
      k temp1 
  let fresh_1 n k fib p1 =
    let temp3 = (n - 2) in
      let temp4 = fib temp3  in
        let temp6 = fresh_2 p1 k in
          temp4 temp6 
  let rec fib n k =
    (if (n < 2)
    then k n 
    else let temp10 = (n - 1) in
           let temp14 = fresh_1 n k fib in
             fib temp10 temp14)
  let main =
    let w = fib 6 id in
      let z = print w  in
        0
$ cat fib2.s | grep -v 'section .note.GNU-stack'
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib_cps.exe
  rukaml_print_int 8

