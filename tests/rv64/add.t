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
      14	  addi sp, sp, -8 #  for func closure
      15	  addi sp, sp, -8 # alloc space for RA register
      16	  lla a0, add
      17	  li a1, 2
      18	  sd ra, (sp)
      19	  call rukaml_alloc_closure
      20	  ld ra, (sp)
      21	  addi sp, sp, 8 # free space of RA register
      22	  sd a0, (sp)
      23	# Allocate args to call fun "add" arguments
      24	  addi sp, sp, -8
      25	  li t0, 1
      26	  sd t0, (sp) # constant
      27	  addi sp, sp, -8 # alloc space for RA register
      28	  ld a0, 16(sp)
      29	  li a1, 1
      30	  ld a2, 8(sp) # arg 0
      31	  sd ra, (sp)
      32	  call rukaml_applyN
      33	  sd a0, 40(sp)
      34	  ld ra, (sp)
      35	  addi sp, sp, 8 # free space of RA register
      36	  addi sp, sp, 8 # deallocate 1 args
      37	  addi sp, sp, 8 # deallocate closure value
      38	  addi sp, sp, -16 # RA and 1st arg of function temp2
      39	  sd ra, 8(sp)
      40	  li t0, 10
      41	  sd t0, (sp)
      42	  ld a0, 32(sp)
      43	  li a1, 1
      44	  ld a2, (sp)
      45	  call rukaml_applyN
      46	  ld ra, 8(sp)
      47	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp2"
      48	  sd a0, 8(sp)
      49	  addi sp, sp, -16
      50	  sd ra, 8(sp)
      51	  ld t0, 24(sp)
      52	  sd t0, (sp)
      53	  call rukaml_print_int
      54	  ld ra, 8(sp)
      55	  sd a0, 16(sp)
      56	  addi sp, sp, 16
      57	  li a0, 0
      58	  addi sp, sp, 24 # deallocate local variables y, x, temp2
      59	  addi a0, x0, 0 # Use 0 return code
      60	  addi a7, x0, 93 # Service command code 93 terminates
      61	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib_acc.exe
  rukaml_print_int 11
