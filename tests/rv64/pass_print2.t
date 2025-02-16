  $ cat pass_print2.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let addk a b k =
    let temp1 = (a + b) in
      k temp1 
  let foo f x =
    f x 
  let main =
    let tmp = foo print  in
      let temp5 = addk 1  in
        let temp6 = temp5 10  in
          temp6 tmp 
  ANF: let addk a b k =
         let temp1 = (a + b) in
           k temp1 
       let foo f x =
         f x 
       let main =
         let tmp = foo print  in
           let temp5 = addk 1  in
             let temp6 = temp5 10  in
               temp6 tmp 
  Location argument "k" in [rbp+2]
  Location argument "b" in [rbp+1]
  Location argument "a" in [rbp+0]
  Removing info about args [ a, b, k ]
  Location argument "x" in [rbp+1]
  Location argument "f" in [rbp+0]
  Removing info about args [ f, x ]
  Removing info about args [  ]
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl addk
       2	addk:
       3	  addi sp, sp, -8 # allocate for local variables temp1
       4	# a is stored in 0
       5	# b is stored in -1
       6	# last_pos = 1
       7	  ld t3, 8(sp)
       8	  ld t4, 16(sp)
       9	  add  t5, t3, t4
      10	  sd t5, (sp)
      11	  addi sp, sp, -8 # first arg of a function k
      12	  ld t5, 8(sp)
      13	  sd t5, (sp) # access a var "temp1"
      14	  addi sp, sp, -8 # alloc space for RA register
      15	  ld a0, 40(sp)
      16	  li a1, 1
      17	  ld a2, 8(sp)
      18	  sd ra, (sp)
      19	  call rukaml_applyN
      20	  ld ra, (sp)
      21	  addi sp, sp, 8 # free space of RA register
      22	  addi sp, sp, 8 # free space for args of function "k"
      23	  addi sp, sp, 8 # deallocate local variables temp1
      24	  ret # addk
      25	.globl foo
      26	foo:
      27	  addi sp, sp, -8 # first arg of a function f
      28	  ld t5, 16(sp)
      29	  sd t5, (sp) # access a var "x"
      30	  addi sp, sp, -8 # alloc space for RA register
      31	  ld a0, 16(sp)
      32	  li a1, 1
      33	  ld a2, 8(sp)
      34	  sd ra, (sp)
      35	  call rukaml_applyN
      36	  ld ra, (sp)
      37	  addi sp, sp, 8 # free space of RA register
      38	  addi sp, sp, 8 # free space for args of function "f"
      39	  ret # foo
      40	.globl main
      41	main:
      42	  addi sp, sp, -24 # allocate for local variables temp6, temp5, tmp
      43	  addi sp, sp, -8 #  for func closure
      44	  addi sp, sp, -8 # alloc space for RA register
      45	  lla a0, foo
      46	  li a1, 2
      47	  sd ra, (sp)
      48	  call rukaml_alloc_closure
      49	  ld ra, (sp)
      50	  addi sp, sp, 8 # free space of RA register
      51	  sd a0, (sp)
      52	# Allocate args to call fun "foo" arguments
      53	  addi sp, sp, -8
      54	  lla a0, rukaml_print_int
      55	  li a1, 1
      56	  call rukaml_alloc_closure
      57	  sd a0, (sp)
      58	  addi sp, sp, -8 # alloc space for RA register
      59	  ld a0, 16(sp)
      60	  li a1, 1
      61	  ld a2, 8(sp) # arg 0
      62	  sd ra, (sp)
      63	  call rukaml_applyN
      64	  sd a0, 40(sp)
      65	  ld ra, (sp)
      66	  addi sp, sp, 8 # free space of RA register
      67	  addi sp, sp, 8 # deallocate 1 args
      68	  addi sp, sp, 8 # deallocate closure value
      69	  addi sp, sp, -8 #  for func closure
      70	  addi sp, sp, -8 # alloc space for RA register
      71	  lla a0, addk
      72	  li a1, 3
      73	  sd ra, (sp)
      74	  call rukaml_alloc_closure
      75	  ld ra, (sp)
      76	  addi sp, sp, 8 # free space of RA register
      77	  sd a0, (sp)
      78	# Allocate args to call fun "addk" arguments
      79	  addi sp, sp, -8
      80	  li t0, 1
      81	  sd t0, (sp) # constant
      82	  addi sp, sp, -8 # alloc space for RA register
      83	  ld a0, 16(sp)
      84	  li a1, 1
      85	  ld a2, 8(sp) # arg 0
      86	  sd ra, (sp)
      87	  call rukaml_applyN
      88	  sd a0, 32(sp)
      89	  ld ra, (sp)
      90	  addi sp, sp, 8 # free space of RA register
      91	  addi sp, sp, 8 # deallocate 1 args
      92	  addi sp, sp, 8 # deallocate closure value
      93	  addi sp, sp, -8 # first arg of a function temp5
      94	  li t0, 10
      95	  sd t0, (sp)
      96	  addi sp, sp, -8 # alloc space for RA register
      97	  ld a0, 24(sp)
      98	  li a1, 1
      99	  ld a2, 8(sp)
     100	  sd ra, (sp)
     101	  call rukaml_applyN
     102	  ld ra, (sp)
     103	  addi sp, sp, 8 # free space of RA register
     104	  addi sp, sp, 8 # free space for args of function "temp5"
     105	  sd a0, (sp)
     106	  addi sp, sp, -8 # first arg of a function temp6
     107	  ld t5, 24(sp)
     108	  sd t5, (sp) # access a var "tmp"
     109	  addi sp, sp, -8 # alloc space for RA register
     110	  ld a0, 16(sp)
     111	  li a1, 1
     112	  ld a2, 8(sp)
     113	  sd ra, (sp)
     114	  call rukaml_applyN
     115	  ld ra, (sp)
     116	  addi sp, sp, 8 # free space of RA register
     117	  addi sp, sp, 8 # free space for args of function "temp6"
     118	  addi sp, sp, 24 # deallocate local variables temp6, temp5, tmp
     119	  addi a0, x0, 0 # Use 0 return code
     120	  addi a7, x0, 93 # Service command code 93 terminates
     121	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
  rukaml_print_int 11
