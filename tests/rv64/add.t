  $ cat add.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let add a b =
    (a + b)
  let main =
    let temp2 = add 1  in
      let x = temp2 10  in
        let y = print x  in
          0
  ANF: let add a b =
         (a + b)
       let main =
         let temp2 = add 1  in
           let x = temp2 10  in
             let y = print x  in
               0
  Location argument "b" in [rbp+1]
  Location argument "a" in [rbp+0]
  Removing info about args [ a, b ]
  Removing info about args [  ]
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl add
       2	add:
       3	# a is stored in 0
       4	# b is stored in -1
       5	# last_pos = 0
       6	  ld t3, (sp)
       7	  ld t4, 8(sp)
       8	  add  t5, t3, t4
       9	  addi a0, t5, 0
      10	  ret # add
      11	.globl main
      12	main:
      13	  addi sp, sp, -24 # allocate for local variables y, x, temp2
      14	  addi sp, sp, -16 #  RA + closure
      15	  sd ra, 8(sp)
      16	  lla a0, add
      17	  li a1, 2
      18	  call rukaml_alloc_closure
      19	  sd a0, (sp)
      20	# Allocate args to call fun "add" arguments
      21	  addi sp, sp, -8
      22	  li t0, 1
      23	  sd t0, (sp) # constant
      24	  ld a0, 8(sp)
      25	  li a1, 1
      26	  ld a2, (sp) # arg 0
      27	  call rukaml_applyN
      28	  sd a0, 40(sp)
      29	  addi sp, sp, 8 # deallocate 1 args
      30	  ld ra, 8(sp)
      31	  addi sp, sp, 16 # deallocate RA + closure
      32	  addi sp, sp, -16 # RA and 1st arg of function temp2
      33	  sd ra, 8(sp)
      34	  li t0, 10
      35	  sd t0, (sp)
      36	  ld a0, 32(sp)
      37	  li a1, 1
      38	  ld a2, (sp)
      39	  call rukaml_applyN
      40	  ld ra, 8(sp)
      41	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp2"
      42	  sd a0, 8(sp)
      43	  addi sp, sp, -16
      44	  sd ra, 8(sp)
      45	  ld t0, 24(sp)
      46	  sd t0, (sp)
      47	  call rukaml_print_int
      48	  ld ra, 8(sp)
      49	  sd a0, 16(sp)
      50	  addi sp, sp, 16
      51	  li a0, 0
      52	  addi sp, sp, 24 # deallocate local variables y, x, temp2
      53	  addi a0, x0, 0 # Use 0 return code
      54	  addi a7, x0, 93 # Service command code 93 terminates
      55	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib_acc.exe
  rukaml_print_int 11
