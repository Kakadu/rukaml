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
      31	  addi sp, sp, -8 #  for func closure
      32	  addi sp, sp, -8 # alloc space for RA register
      33	  lla a0, fac
      34	  li a1, 2
      35	  sd ra, (sp)
      36	  call rukaml_alloc_closure
      37	  ld ra, (sp)
      38	  addi sp, sp, 8 # free space of RA register
      39	  sd a0, (sp)
      40	# Allocate args to call fun "fac" arguments
      41	  addi sp, sp, -8
      42	  ld t0, 24(sp) # arg "p1"
      43	  sd t0, (sp)
      44	  addi sp, sp, -8 # alloc space for RA register
      45	  ld a0, 16(sp)
      46	  li a1, 1
      47	  ld a2, 8(sp) # arg 0
      48	  sd ra, (sp)
      49	  call rukaml_applyN
      50	  sd a0, 24(sp)
      51	  ld ra, (sp)
      52	  addi sp, sp, 8 # free space of RA register
      53	  addi sp, sp, 8 # deallocate 1 args
      54	  addi sp, sp, 8 # deallocate closure value
      55	  addi sp, sp, -16 # RA and 1st arg of function temp5
      56	  sd ra, 8(sp)
      57	  ld t5, 32(sp)
      58	  sd t5, (sp) # access a var "n1"
      59	  ld a0, 16(sp)
      60	  li a1, 1
      61	  ld a2, (sp)
      62	  call rukaml_applyN
      63	  ld ra, 8(sp)
      64	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp5"
      65	lab_endif_13:
      66	  addi sp, sp, 32 # deallocate local variables temp5, p1, n1, temp1
      67	  ret # fac
      68	.globl main
      69	main:
      70	  addi sp, sp, -24 # allocate for local variables g, f, temp7
      71	  addi sp, sp, -8 #  for func closure
      72	  addi sp, sp, -8 # alloc space for RA register
      73	  lla a0, fac
      74	  li a1, 2
      75	  sd ra, (sp)
      76	  call rukaml_alloc_closure
      77	  ld ra, (sp)
      78	  addi sp, sp, 8 # free space of RA register
      79	  sd a0, (sp)
      80	# Allocate args to call fun "fac" arguments
      81	  addi sp, sp, -8
      82	  li t0, 1
      83	  sd t0, (sp) # constant
      84	  addi sp, sp, -8 # alloc space for RA register
      85	  ld a0, 16(sp)
      86	  li a1, 1
      87	  ld a2, 8(sp) # arg 0
      88	  sd ra, (sp)
      89	  call rukaml_applyN
      90	  sd a0, 40(sp)
      91	  ld ra, (sp)
      92	  addi sp, sp, 8 # free space of RA register
      93	  addi sp, sp, 8 # deallocate 1 args
      94	  addi sp, sp, 8 # deallocate closure value
      95	  addi sp, sp, -16 # RA and 1st arg of function temp7
      96	  sd ra, 8(sp)
      97	  li t0, 4
      98	  sd t0, (sp)
      99	  ld a0, 32(sp)
     100	  li a1, 1
     101	  ld a2, (sp)
     102	  call rukaml_applyN
     103	  ld ra, 8(sp)
     104	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp7"
     105	  sd a0, 8(sp)
     106	  addi sp, sp, -16
     107	  sd ra, 8(sp)
     108	  ld t0, 24(sp)
     109	  sd t0, (sp)
     110	  call rukaml_print_int
     111	  ld ra, 8(sp)
     112	  sd a0, 16(sp)
     113	  addi sp, sp, 16
     114	  li a0, 0
     115	  addi sp, sp, 24 # deallocate local variables g, f, temp7
     116	  addi a0, x0, 0 # Use 0 return code
     117	  addi a7, x0, 93 # Service command code 93 terminates
     118	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fac_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fac_acc.exe
  rukaml_print_int 24
