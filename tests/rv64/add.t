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
      14	  addi sp, sp, -8 # alloc space for RA register
      15	  lla a0, add
      16	  li a1, 2
      17	  sd ra, (sp)
      18	  call rukaml_alloc_closure
      19	  ld ra, (sp)
      20	  addi sp, sp, 8 # free space of RA register
      21	# Allocate args to call fun "add" arguments
      22	  addi sp, sp, -8
      23	  li t0, 1
      24	  sd t0, (sp) # constant
      25	  addi sp, sp, -8 # alloc space for RA register
      26	  li a1, 1
      27	  ld a2, 8(sp) # arg 0
      28	  sd ra, (sp)
      29	  call rukaml_applyN
      30	  sd a0, 32(sp)
      31	  ld ra, (sp)
      32	  addi sp, sp, 8 # free space of RA register
      33	  addi sp, sp, 8 # deallocate 1 args
      34	  addi sp, sp, -8 # first arg of a function temp2
      35	  li t0, 10
      36	  sd t0, (sp)
      37	  addi sp, sp, -8 # alloc space for RA register
      38	  ld a0, 32(sp)
      39	  li a1, 1
      40	  ld a2, 8(sp)
      41	  sd ra, (sp)
      42	  call rukaml_applyN
      43	  ld ra, (sp)
      44	  addi sp, sp, 8 # free space of RA register
      45	  addi sp, sp, 8 # free space for args of function "temp2"
      46	  sd a0, 8(sp)
      47	  addi sp, sp, -8 # alloc space for RA register
      48	  ld a0, 16(sp)
      49	  sd ra, (sp)
      50	  call rukaml_print_int
      51	  sd a0, 8(sp)
      52	  ld ra, (sp)
      53	  addi sp, sp, 8 # free space of RA register
      54	  li a0, 0
      55	  addi sp, sp, 24 # deallocate local variables y, x, temp2
      56	  addi a0, x0, 0 # Use 0 return code
      57	  addi a7, x0, 93 # Service command code 93 terminates
      58	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o  
$ riscv64-linux-gnu-gcc -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib_acc.exe 
  ;
  rukaml_print_int 11
