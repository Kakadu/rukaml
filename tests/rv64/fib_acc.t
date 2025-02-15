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
      55	  addi sp, sp, -8 # first arg of a function temp5
      56	  ld t5, 24(sp)
      57	  sd t5, (sp) # access a var "ab"
      58	  addi sp, sp, -8 # alloc space for RA register
      59	  ld a0, 24(sp)
      60	  li a1, 1
      61	  ld a2, 8(sp)
      62	  sd ra, (sp)
      63	  call rukaml_applyN
      64	  ld ra, (sp)
      65	  addi sp, sp, 8 # free space of RA register
      66	  addi sp, sp, 8 # free space for args of function "temp5"
      67	  sd a0, (sp)
      68	  addi sp, sp, -8 # first arg of a function temp6
      69	  ld t5, 32(sp)
      70	  sd t5, (sp) # access a var "n1"
      71	  addi sp, sp, -8 # alloc space for RA register
      72	  ld a0, 16(sp)
      73	  li a1, 1
      74	  ld a2, 8(sp)
      75	  sd ra, (sp)
      76	  call rukaml_applyN
      77	  ld ra, (sp)
      78	  addi sp, sp, 8 # free space of RA register
      79	  addi sp, sp, 8 # free space for args of function "temp6"
      80	lab_endif_15:
      81	  addi sp, sp, 40 # deallocate local variables temp6, temp5, ab, n1, temp1
      82	  ret # fib_acc
      83	.globl main
      84	main:
      85	  addi sp, sp, -32 # allocate for local variables g, f, temp9, temp8
      86	  addi sp, sp, -8 #  for func closure
      87	  addi sp, sp, -8 # alloc space for RA register
      88	  lla a0, fib_acc
      89	  li a1, 3
      90	  sd ra, (sp)
      91	  call rukaml_alloc_closure
      92	  ld ra, (sp)
      93	  addi sp, sp, 8 # free space of RA register
      94	  sd a0, (sp)
      95	# Allocate args to call fun "fib_acc" arguments
      96	  addi sp, sp, -8
      97	  li t0, 0
      98	  sd t0, (sp) # constant
      99	  addi sp, sp, -8 # alloc space for RA register
     100	  ld a0, 16(sp)
     101	  li a1, 1
     102	  ld a2, 8(sp) # arg 0
     103	  sd ra, (sp)
     104	  call rukaml_applyN
     105	  sd a0, 48(sp)
     106	  ld ra, (sp)
     107	  addi sp, sp, 8 # free space of RA register
     108	  addi sp, sp, 8 # deallocate 1 args
     109	  addi sp, sp, 8 # deallocate closure value
     110	  addi sp, sp, -8 # first arg of a function temp8
     111	  li t0, 1
     112	  sd t0, (sp)
     113	  addi sp, sp, -8 # alloc space for RA register
     114	  ld a0, 40(sp)
     115	  li a1, 1
     116	  ld a2, 8(sp)
     117	  sd ra, (sp)
     118	  call rukaml_applyN
     119	  ld ra, (sp)
     120	  addi sp, sp, 8 # free space of RA register
     121	  addi sp, sp, 8 # free space for args of function "temp8"
     122	  sd a0, 16(sp)
     123	  addi sp, sp, -8 # first arg of a function temp9
     124	  li t0, 4
     125	  sd t0, (sp)
     126	  addi sp, sp, -8 # alloc space for RA register
     127	  ld a0, 32(sp)
     128	  li a1, 1
     129	  ld a2, 8(sp)
     130	  sd ra, (sp)
     131	  call rukaml_applyN
     132	  ld ra, (sp)
     133	  addi sp, sp, 8 # free space of RA register
     134	  addi sp, sp, 8 # free space for args of function "temp9"
     135	  sd a0, 8(sp)
     136	  addi sp, sp, -8 # alloc space for RA register
     137	  sd ra, (sp)
     138	  ld a0, 16(sp)
     139	  addi sp, sp, -8
     140	  sd a0, (sp)
     141	  call rukaml_print_int
     142	  addi sp, sp, 8
     143	  sd a0, 8(sp)
     144	  ld ra, (sp)
     145	  addi sp, sp, 8 # free space of RA register
     146	  li a0, 0
     147	  addi sp, sp, 32 # deallocate local variables g, f, temp9, temp8
     148	  addi a0, x0, 0 # Use 0 return code
     149	  addi a7, x0, 93 # Service command code 93 terminates
     150	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fib_acc.exe
  rukaml_print_int 3
