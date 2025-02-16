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
      55	  addi sp, sp, -8 # first arg of a function temp5
      56	  ld t5, 24(sp)
      57	  sd t5, (sp) # access a var "n1"
      58	  addi sp, sp, -8 # alloc space for RA register
      59	  ld a0, 16(sp)
      60	  li a1, 1
      61	  ld a2, 8(sp)
      62	  sd ra, (sp)
      63	  call rukaml_applyN
      64	  ld ra, (sp)
      65	  addi sp, sp, 8 # free space of RA register
      66	  addi sp, sp, 8 # free space for args of function "temp5"
      67	lab_endif_13:
      68	  addi sp, sp, 32 # deallocate local variables temp5, p1, n1, temp1
      69	  ret # fac
      70	.globl main
      71	main:
      72	  addi sp, sp, -24 # allocate for local variables g, f, temp7
      73	  addi sp, sp, -8 #  for func closure
      74	  addi sp, sp, -8 # alloc space for RA register
      75	  lla a0, fac
      76	  li a1, 2
      77	  sd ra, (sp)
      78	  call rukaml_alloc_closure
      79	  ld ra, (sp)
      80	  addi sp, sp, 8 # free space of RA register
      81	  sd a0, (sp)
      82	# Allocate args to call fun "fac" arguments
      83	  addi sp, sp, -8
      84	  li t0, 1
      85	  sd t0, (sp) # constant
      86	  addi sp, sp, -8 # alloc space for RA register
      87	  ld a0, 16(sp)
      88	  li a1, 1
      89	  ld a2, 8(sp) # arg 0
      90	  sd ra, (sp)
      91	  call rukaml_applyN
      92	  sd a0, 40(sp)
      93	  ld ra, (sp)
      94	  addi sp, sp, 8 # free space of RA register
      95	  addi sp, sp, 8 # deallocate 1 args
      96	  addi sp, sp, 8 # deallocate closure value
      97	  addi sp, sp, -8 # first arg of a function temp7
      98	  li t0, 4
      99	  sd t0, (sp)
     100	  addi sp, sp, -8 # alloc space for RA register
     101	  ld a0, 32(sp)
     102	  li a1, 1
     103	  ld a2, 8(sp)
     104	  sd ra, (sp)
     105	  call rukaml_applyN
     106	  ld ra, (sp)
     107	  addi sp, sp, 8 # free space of RA register
     108	  addi sp, sp, 8 # free space for args of function "temp7"
     109	  sd a0, 8(sp)
     110	  addi sp, sp, -8 # alloc space for RA register
     111	  sd ra, (sp)
     112	  ld a0, 16(sp)
     113	  addi sp, sp, -8
     114	  sd a0, (sp)
     115	  call rukaml_print_int
     116	  addi sp, sp, 8
     117	  sd a0, 8(sp)
     118	  ld ra, (sp)
     119	  addi sp, sp, 8 # free space of RA register
     120	  li a0, 0
     121	  addi sp, sp, 24 # deallocate local variables g, f, temp7
     122	  addi a0, x0, 0 # Use 0 return code
     123	  addi a7, x0, 93 # Service command code 93 terminates
     124	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fac_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fac_acc.exe
  rukaml_print_int 24
