  $ cat fac_acc.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let rec fac acc n =
    let temp1 = (n < 2) in
      (if temp1
      then acc
      else let n1 = (n - 1) in
             let p1 = (acc * n) in
               let temp5 = fac p1  in
                 temp5 n1 )
  let main =
    let temp7 = fac 1  in
      let f = temp7 4  in
        let g = print f  in
          0
  ANF: let rec fac acc n =
         let temp1 = (n < 2) in
           (if temp1
           then acc
           else let n1 = (n - 1) in
                  let p1 = (acc * n) in
                    let temp5 = fac p1  in
                      temp5 n1 )
       let main =
         let temp7 = fac 1  in
           let f = temp7 4  in
             let g = print f  in
               0
  Location argument "n" in [rbp+1]
  Location argument "acc" in [rbp+0]
  Removing info about args [ acc, n ]
  Removing info about args [  ]
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl fac
       2	fac:
       3	  addi sp, sp, -32 # allocate for local variables temp5, p1, n1, temp1
       4	# 40(sp), find_exn "n" = -1, last_pos = 4
       5	  ld t0, 40(sp) # locals = 4
       6	  li t1, 2
       7	  blt t0, t1, lab_10
       8	  sd zero, 24(sp)
       9	  beq zero, zero, lab_11
      10	lab_10:
      11	  li t0, 1
      12	  sd t0, 24(sp) # dest = 24(sp)
      13	  beq zero, zero, lab_11
      14	lab_11:
      15	  ld t0, 24(sp)
      16	  beq t0, zero, lab_else_12
      17	  ld t5, 32(sp)
      18	  addi a0, t5, 0
      19	  beq zero, zero, lab_endif_13
      20	lab_else_12: # temp1 is 0
      21	  ld t0, 40(sp)
      22	  addi t0, t0, -1
      23	  sd t0, 16(sp)
      24	# acc is stored in 0
      25	# n is stored in -1
      26	# last_pos = 4
      27	  ld t3, 32(sp)
      28	  ld t4, 40(sp)
      29	  mulw t5, t3, t4
      30	  sd t5, 8(sp)
      31	  addi sp, sp, -8 # alloc space for RA register
      32	  lla a0, fac
      33	  li a1, 2
      34	  sd ra, (sp)
      35	  call rukaml_alloc_closure
      36	  ld ra, (sp)
      37	  addi sp, sp, 8 # free space of RA register
      38	# Allocate args to call fun "fac" arguments
      39	  addi sp, sp, -8
      40	  ld t0, 16(sp) # arg "p1"
      41	  sd t0, (sp)
      42	  addi sp, sp, -8 # alloc space for RA register
      43	  li a1, 1
      44	  ld a2, 8(sp) # arg 0
      45	  sd ra, (sp)
      46	  call rukaml_applyN
      47	  sd a0, 16(sp)
      48	  ld ra, (sp)
      49	  addi sp, sp, 8 # free space of RA register
      50	  addi sp, sp, 8 # deallocate 1 args
      51	  addi sp, sp, -8 # first arg of a function temp5
      52	  ld t5, 24(sp)
      53	  sd t5, (sp) # access a var "n1"
      54	  addi sp, sp, -8 # alloc space for RA register
      55	  ld a0, 16(sp)
      56	  li a1, 1
      57	  ld a2, 8(sp)
      58	  sd ra, (sp)
      59	  call rukaml_applyN
      60	  ld ra, (sp)
      61	  addi sp, sp, 8 # free space of RA register
      62	  addi sp, sp, 8 # free space for args of function "temp5"
      63	lab_endif_13:
      64	  addi sp, sp, 32 # deallocate local variables temp5, p1, n1, temp1
      65	  ret # fac
      66	.globl main
      67	main:
      68	  addi sp, sp, -24 # allocate for local variables g, f, temp7
      69	  addi sp, sp, -8 # alloc space for RA register
      70	  lla a0, fac
      71	  li a1, 2
      72	  sd ra, (sp)
      73	  call rukaml_alloc_closure
      74	  ld ra, (sp)
      75	  addi sp, sp, 8 # free space of RA register
      76	# Allocate args to call fun "fac" arguments
      77	  addi sp, sp, -8
      78	  li t0, 1
      79	  sd t0, (sp) # constant
      80	  addi sp, sp, -8 # alloc space for RA register
      81	  li a1, 1
      82	  ld a2, 8(sp) # arg 0
      83	  sd ra, (sp)
      84	  call rukaml_applyN
      85	  sd a0, 32(sp)
      86	  ld ra, (sp)
      87	  addi sp, sp, 8 # free space of RA register
      88	  addi sp, sp, 8 # deallocate 1 args
      89	  addi sp, sp, -8 # first arg of a function temp7
      90	  li t0, 4
      91	  sd t0, (sp)
      92	  addi sp, sp, -8 # alloc space for RA register
      93	  ld a0, 32(sp)
      94	  li a1, 1
      95	  ld a2, 8(sp)
      96	  sd ra, (sp)
      97	  call rukaml_applyN
      98	  ld ra, (sp)
      99	  addi sp, sp, 8 # free space of RA register
     100	  addi sp, sp, 8 # free space for args of function "temp7"
     101	  sd a0, 8(sp)
     102	  addi sp, sp, -8 # alloc space for RA register
     103	  ld a0, 16(sp)
     104	  sd ra, (sp)
     105	  call rukaml_print_int
     106	  sd a0, 8(sp)
     107	  ld ra, (sp)
     108	  addi sp, sp, 8 # free space of RA register
     109	  li a0, 0
     110	  addi sp, sp, 24 # deallocate local variables g, f, temp7
     111	  addi a0, x0, 0 # Use 0 return code
     112	  addi a7, x0, 93 # Service command code 93 terminates
     113	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o  
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fac_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fac_acc.exe 
  H
  rukaml_print_int 24
