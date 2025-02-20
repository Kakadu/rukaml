  $ cat fib_acc.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let rec fib_acc a b n =
    let temp1 = (n = 1) in
      (if temp1
      then b
      else let n1 = (n - 1) in
             let ab = (a + b) in
               let temp5 = fib_acc b  in
                 let temp6 = temp5 ab  in
                   temp6 n1 )
  let main =
    let temp8 = fib_acc 0  in
      let temp9 = temp8 1  in
        let f = temp9 4  in
          let g = print f  in
            0
  ANF: let rec fib_acc a b n =
         let temp1 = (n = 1) in
           (if temp1
           then b
           else let n1 = (n - 1) in
                  let ab = (a + b) in
                    let temp5 = fib_acc b  in
                      let temp6 = temp5 ab  in
                        temp6 n1 )
       let main =
         let temp8 = fib_acc 0  in
           let temp9 = temp8 1  in
             let f = temp9 4  in
               let g = print f  in
                 0
  Location argument "n" in [rbp+2]
  Location argument "b" in [rbp+1]
  Location argument "a" in [rbp+0]
  Removing info about args [ a, b, n ]
  Removing info about args [  ]
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl fib_acc
       2	fib_acc:
       3	  addi sp, sp, -40 # allocate for local variables temp6, temp5, ab, n1, temp1
       4	# 56(sp), find_exn "n" = -2, last_pos = 5
       5	  ld t0, 56(sp) # locals = 5
       6	  li t1, 1
       7	  beq t0, t1, lab_12
       8	  sd zero, 32(sp)
       9	  beq zero, zero, lab_13
      10	lab_12:
      11	  li t0, 1
      12	  sd t0, 32(sp) # dest = 32(sp)
      13	  beq zero, zero, lab_13
      14	lab_13:
      15	  ld t0, 32(sp)
      16	  beq t0, zero, lab_else_14
      17	  ld t5, 48(sp)
      18	  addi a0, t5, 0
      19	  beq zero, zero, lab_endif_15
      20	lab_else_14: # temp1 is 0
      21	  ld t0, 56(sp)
      22	  addi t0, t0, -1
      23	  sd t0, 24(sp)
      24	# a is stored in 0
      25	# b is stored in -1
      26	# last_pos = 5
      27	  ld t3, 40(sp)
      28	  ld t4, 48(sp)
      29	  add  t5, t3, t4
      30	  sd t5, 16(sp)
      31	  addi sp, sp, -8 #  for func closure
      32	  addi sp, sp, -8 # alloc space for RA register
      33	  lla a0, fib_acc
      34	  li a1, 3
      35	  sd ra, (sp)
      36	  call rukaml_alloc_closure
      37	  ld ra, (sp)
      38	  addi sp, sp, 8 # free space of RA register
      39	  sd a0, (sp)
      40	# Allocate args to call fun "fib_acc" arguments
      41	  addi sp, sp, -8
      42	  ld t0, 64(sp) # arg "b"
      43	  sd t0, (sp)
      44	  addi sp, sp, -8 # alloc space for RA register
      45	  ld a0, 16(sp)
      46	  li a1, 1
      47	  ld a2, 8(sp) # arg 0
      48	  sd ra, (sp)
      49	  call rukaml_applyN
      50	  sd a0, 32(sp)
      51	  ld ra, (sp)
      52	  addi sp, sp, 8 # free space of RA register
      53	  addi sp, sp, 8 # deallocate 1 args
      54	  addi sp, sp, 8 # deallocate closure value
      55	  addi sp, sp, -16 # RA and 1st arg of function temp5
      56	  sd ra, 8(sp)
      57	  ld t5, 32(sp)
      58	  sd t5, (sp) # access a var "ab"
      59	  ld a0, 24(sp)
      60	  li a1, 1
      61	  ld a2, (sp)
      62	  call rukaml_applyN
      63	  ld ra, 8(sp)
      64	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp5"
      65	  sd a0, (sp)
      66	  addi sp, sp, -16 # RA and 1st arg of function temp6
      67	  sd ra, 8(sp)
      68	  ld t5, 40(sp)
      69	  sd t5, (sp) # access a var "n1"
      70	  ld a0, 16(sp)
      71	  li a1, 1
      72	  ld a2, (sp)
      73	  call rukaml_applyN
      74	  ld ra, 8(sp)
      75	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp6"
      76	lab_endif_15:
      77	  addi sp, sp, 40 # deallocate local variables temp6, temp5, ab, n1, temp1
      78	  ret # fib_acc
      79	.globl main
      80	main:
      81	  addi sp, sp, -32 # allocate for local variables g, f, temp9, temp8
      82	  addi sp, sp, -8 #  for func closure
      83	  addi sp, sp, -8 # alloc space for RA register
      84	  lla a0, fib_acc
      85	  li a1, 3
      86	  sd ra, (sp)
      87	  call rukaml_alloc_closure
      88	  ld ra, (sp)
      89	  addi sp, sp, 8 # free space of RA register
      90	  sd a0, (sp)
      91	# Allocate args to call fun "fib_acc" arguments
      92	  addi sp, sp, -8
      93	  li t0, 0
      94	  sd t0, (sp) # constant
      95	  addi sp, sp, -8 # alloc space for RA register
      96	  ld a0, 16(sp)
      97	  li a1, 1
      98	  ld a2, 8(sp) # arg 0
      99	  sd ra, (sp)
     100	  call rukaml_applyN
     101	  sd a0, 48(sp)
     102	  ld ra, (sp)
     103	  addi sp, sp, 8 # free space of RA register
     104	  addi sp, sp, 8 # deallocate 1 args
     105	  addi sp, sp, 8 # deallocate closure value
     106	  addi sp, sp, -16 # RA and 1st arg of function temp8
     107	  sd ra, 8(sp)
     108	  li t0, 1
     109	  sd t0, (sp)
     110	  ld a0, 40(sp)
     111	  li a1, 1
     112	  ld a2, (sp)
     113	  call rukaml_applyN
     114	  ld ra, 8(sp)
     115	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp8"
     116	  sd a0, 16(sp)
     117	  addi sp, sp, -16 # RA and 1st arg of function temp9
     118	  sd ra, 8(sp)
     119	  li t0, 4
     120	  sd t0, (sp)
     121	  ld a0, 32(sp)
     122	  li a1, 1
     123	  ld a2, (sp)
     124	  call rukaml_applyN
     125	  ld ra, 8(sp)
     126	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp9"
     127	  sd a0, 8(sp)
     128	  addi sp, sp, -16
     129	  sd ra, 8(sp)
     130	  ld t0, 24(sp)
     131	  sd t0, (sp)
     132	  call rukaml_print_int
     133	  ld ra, 8(sp)
     134	  sd a0, 16(sp)
     135	  addi sp, sp, 16
     136	  li a0, 0
     137	  addi sp, sp, 32 # deallocate local variables g, f, temp9, temp8
     138	  addi a0, x0, 0 # Use 0 return code
     139	  addi a7, x0, 93 # Service command code 93 terminates
     140	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fib_acc.exe
  rukaml_print_int 3
