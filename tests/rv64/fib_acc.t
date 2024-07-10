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
      31	  addi sp, sp, -8 # alloc space for RA register
      32	  lla a0, fib_acc
      33	  li a1, 3
      34	  sd ra, (sp)
      35	  call rukaml_alloc_closure
      36	  ld ra, (sp)
      37	  addi sp, sp, 8 # free space of RA register
      38	# Allocate args to call fun "fib_acc" arguments
      39	  addi sp, sp, -8
      40	  ld t0, 56(sp) # arg "b"
      41	  sd t0, (sp)
      42	  addi sp, sp, -8 # alloc space for RA register
      43	  li a1, 1
      44	  ld a2, 8(sp) # arg 0
      45	  sd ra, (sp)
      46	  call rukaml_applyN
      47	  sd a0, 24(sp)
      48	  ld ra, (sp)
      49	  addi sp, sp, 8 # free space of RA register
      50	  addi sp, sp, 8 # deallocate 1 args
      51	  addi sp, sp, -8 # first arg of a function temp5
      52	  ld t5, 24(sp)
      53	  sd t5, (sp) # access a var "ab"
      54	  addi sp, sp, -8 # alloc space for RA register
      55	  ld a0, 24(sp)
      56	  li a1, 1
      57	  ld a2, 8(sp)
      58	  sd ra, (sp)
      59	  call rukaml_applyN
      60	  ld ra, (sp)
      61	  addi sp, sp, 8 # free space of RA register
      62	  addi sp, sp, 8 # free space for args of function "temp5"
      63	  sd a0, (sp)
      64	  addi sp, sp, -8 # first arg of a function temp6
      65	  ld t5, 32(sp)
      66	  sd t5, (sp) # access a var "n1"
      67	  addi sp, sp, -8 # alloc space for RA register
      68	  ld a0, 16(sp)
      69	  li a1, 1
      70	  ld a2, 8(sp)
      71	  sd ra, (sp)
      72	  call rukaml_applyN
      73	  ld ra, (sp)
      74	  addi sp, sp, 8 # free space of RA register
      75	  addi sp, sp, 8 # free space for args of function "temp6"
      76	lab_endif_15:
      77	  addi sp, sp, 40 # deallocate local variables temp6, temp5, ab, n1, temp1
      78	  ret # fib_acc
      79	.globl main
      80	main:
      81	  addi sp, sp, -32 # allocate for local variables g, f, temp9, temp8
      82	  addi sp, sp, -8 # alloc space for RA register
      83	  lla a0, fib_acc
      84	  li a1, 3
      85	  sd ra, (sp)
      86	  call rukaml_alloc_closure
      87	  ld ra, (sp)
      88	  addi sp, sp, 8 # free space of RA register
      89	# Allocate args to call fun "fib_acc" arguments
      90	  addi sp, sp, -8
      91	  li t0, 0
      92	  sd t0, (sp) # constant
      93	  addi sp, sp, -8 # alloc space for RA register
      94	  li a1, 1
      95	  ld a2, 8(sp) # arg 0
      96	  sd ra, (sp)
      97	  call rukaml_applyN
      98	  sd a0, 40(sp)
      99	  ld ra, (sp)
     100	  addi sp, sp, 8 # free space of RA register
     101	  addi sp, sp, 8 # deallocate 1 args
     102	  addi sp, sp, -8 # first arg of a function temp8
     103	  li t0, 1
     104	  sd t0, (sp)
     105	  addi sp, sp, -8 # alloc space for RA register
     106	  ld a0, 40(sp)
     107	  li a1, 1
     108	  ld a2, 8(sp)
     109	  sd ra, (sp)
     110	  call rukaml_applyN
     111	  ld ra, (sp)
     112	  addi sp, sp, 8 # free space of RA register
     113	  addi sp, sp, 8 # free space for args of function "temp8"
     114	  sd a0, 16(sp)
     115	  addi sp, sp, -8 # first arg of a function temp9
     116	  li t0, 4
     117	  sd t0, (sp)
     118	  addi sp, sp, -8 # alloc space for RA register
     119	  ld a0, 32(sp)
     120	  li a1, 1
     121	  ld a2, 8(sp)
     122	  sd ra, (sp)
     123	  call rukaml_applyN
     124	  ld ra, (sp)
     125	  addi sp, sp, 8 # free space of RA register
     126	  addi sp, sp, 8 # free space for args of function "temp9"
     127	  sd a0, 8(sp)
     128	  addi sp, sp, -8 # alloc space for RA register
     129	  ld a0, 16(sp)
     130	  sd ra, (sp)
     131	  call rukaml_print_int
     132	  sd a0, 8(sp)
     133	  ld ra, (sp)
     134	  addi sp, sp, 8 # free space of RA register
     135	  li a0, 0
     136	  addi sp, sp, 32 # deallocate local variables g, f, temp9, temp8
     137	  addi a0, x0, 0 # Use 0 return code
     138	  addi a7, x0, 93 # Service command code 93 terminates
     139	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fib_acc.exe
  rukaml_print_int 3
