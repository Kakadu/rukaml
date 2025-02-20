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
      31	  addi sp, sp, -16 #  RA + closure
      32	  sd ra, 8(sp)
      33	  lla a0, fac
      34	  li a1, 2
      35	  call rukaml_alloc_closure
      36	  sd a0, (sp)
      37	# Allocate args to call fun "fac" arguments
      38	  addi sp, sp, -8
      39	  ld t0, 32(sp) # arg "p1"
      40	  sd t0, (sp)
      41	  ld a0, 8(sp)
      42	  li a1, 1
      43	  ld a2, (sp) # arg 0
      44	  call rukaml_applyN
      45	  sd a0, 24(sp)
      46	  addi sp, sp, 8 # deallocate 1 args
      47	  ld ra, 8(sp)
      48	  addi sp, sp, 16 # deallocate RA + closure
      49	  addi sp, sp, -16 # RA and 1st arg of function temp5
      50	  sd ra, 8(sp)
      51	  ld t5, 32(sp)
      52	  sd t5, (sp) # access a var "n1"
      53	  ld a0, 16(sp)
      54	  li a1, 1
      55	  ld a2, (sp)
      56	  call rukaml_applyN
      57	  ld ra, 8(sp)
      58	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp5"
      59	lab_endif_13:
      60	  addi sp, sp, 32 # deallocate local variables temp5, p1, n1, temp1
      61	  ret # fac
      62	.globl main
      63	main:
      64	  addi sp, sp, -24 # allocate for local variables g, f, temp7
      65	  addi sp, sp, -16 #  RA + closure
      66	  sd ra, 8(sp)
      67	  lla a0, fac
      68	  li a1, 2
      69	  call rukaml_alloc_closure
      70	  sd a0, (sp)
      71	# Allocate args to call fun "fac" arguments
      72	  addi sp, sp, -8
      73	  li t0, 1
      74	  sd t0, (sp) # constant
      75	  ld a0, 8(sp)
      76	  li a1, 1
      77	  ld a2, (sp) # arg 0
      78	  call rukaml_applyN
      79	  sd a0, 40(sp)
      80	  addi sp, sp, 8 # deallocate 1 args
      81	  ld ra, 8(sp)
      82	  addi sp, sp, 16 # deallocate RA + closure
      83	  addi sp, sp, -16 # RA and 1st arg of function temp7
      84	  sd ra, 8(sp)
      85	  li t0, 4
      86	  sd t0, (sp)
      87	  ld a0, 32(sp)
      88	  li a1, 1
      89	  ld a2, (sp)
      90	  call rukaml_applyN
      91	  ld ra, 8(sp)
      92	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp7"
      93	  sd a0, 8(sp)
      94	  addi sp, sp, -16
      95	  sd ra, 8(sp)
      96	  ld t0, 24(sp)
      97	  sd t0, (sp)
      98	  call rukaml_print_int
      99	  ld ra, 8(sp)
     100	  sd a0, 16(sp)
     101	  addi sp, sp, 16
     102	  li a0, 0
     103	  addi sp, sp, 24 # deallocate local variables g, f, temp7
     104	  addi a0, x0, 0 # Use 0 return code
     105	  addi a7, x0, 93 # Service command code 93 terminates
     106	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fac_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fac_acc.exe
  rukaml_print_int 24
