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
      11	  addi sp, sp, -16 # RA and 1st arg of function k
      12	  sd ra, 8(sp)
      13	  ld t5, 16(sp)
      14	  sd t5, (sp) # access a var "temp1"
      15	  ld a0, 40(sp)
      16	  li a1, 1
      17	  ld a2, (sp)
      18	  call rukaml_applyN
      19	  ld ra, 8(sp)
      20	  addi sp, sp, 16 # free space for ra and arg 1 of function "k"
      21	  addi sp, sp, 8 # deallocate local variables temp1
      22	  ret # addk
      23	.globl foo
      24	foo:
      25	  addi sp, sp, -16 # RA and 1st arg of function f
      26	  sd ra, 8(sp)
      27	  ld t5, 24(sp)
      28	  sd t5, (sp) # access a var "x"
      29	  ld a0, 16(sp)
      30	  li a1, 1
      31	  ld a2, (sp)
      32	  call rukaml_applyN
      33	  ld ra, 8(sp)
      34	  addi sp, sp, 16 # free space for ra and arg 1 of function "f"
      35	  ret # foo
      36	.globl main
      37	main:
      38	  addi sp, sp, -24 # allocate for local variables temp6, temp5, tmp
      39	  addi sp, sp, -8 #  for func closure
      40	  addi sp, sp, -8 # alloc space for RA register
      41	  lla a0, foo
      42	  li a1, 2
      43	  sd ra, (sp)
      44	  call rukaml_alloc_closure
      45	  ld ra, (sp)
      46	  addi sp, sp, 8 # free space of RA register
      47	  sd a0, (sp)
      48	# Allocate args to call fun "foo" arguments
      49	  addi sp, sp, -8
      50	  lla a0, rukaml_print_int
      51	  li a1, 1
      52	  call rukaml_alloc_closure
      53	  sd a0, (sp)
      54	  addi sp, sp, -8 # alloc space for RA register
      55	  ld a0, 16(sp)
      56	  li a1, 1
      57	  ld a2, 8(sp) # arg 0
      58	  sd ra, (sp)
      59	  call rukaml_applyN
      60	  sd a0, 40(sp)
      61	  ld ra, (sp)
      62	  addi sp, sp, 8 # free space of RA register
      63	  addi sp, sp, 8 # deallocate 1 args
      64	  addi sp, sp, 8 # deallocate closure value
      65	  addi sp, sp, -8 #  for func closure
      66	  addi sp, sp, -8 # alloc space for RA register
      67	  lla a0, addk
      68	  li a1, 3
      69	  sd ra, (sp)
      70	  call rukaml_alloc_closure
      71	  ld ra, (sp)
      72	  addi sp, sp, 8 # free space of RA register
      73	  sd a0, (sp)
      74	# Allocate args to call fun "addk" arguments
      75	  addi sp, sp, -8
      76	  li t0, 1
      77	  sd t0, (sp) # constant
      78	  addi sp, sp, -8 # alloc space for RA register
      79	  ld a0, 16(sp)
      80	  li a1, 1
      81	  ld a2, 8(sp) # arg 0
      82	  sd ra, (sp)
      83	  call rukaml_applyN
      84	  sd a0, 32(sp)
      85	  ld ra, (sp)
      86	  addi sp, sp, 8 # free space of RA register
      87	  addi sp, sp, 8 # deallocate 1 args
      88	  addi sp, sp, 8 # deallocate closure value
      89	  addi sp, sp, -16 # RA and 1st arg of function temp5
      90	  sd ra, 8(sp)
      91	  li t0, 10
      92	  sd t0, (sp)
      93	  ld a0, 24(sp)
      94	  li a1, 1
      95	  ld a2, (sp)
      96	  call rukaml_applyN
      97	  ld ra, 8(sp)
      98	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp5"
      99	  sd a0, (sp)
     100	  addi sp, sp, -16 # RA and 1st arg of function temp6
     101	  sd ra, 8(sp)
     102	  ld t5, 32(sp)
     103	  sd t5, (sp) # access a var "tmp"
     104	  ld a0, 16(sp)
     105	  li a1, 1
     106	  ld a2, (sp)
     107	  call rukaml_applyN
     108	  ld ra, 8(sp)
     109	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp6"
     110	  addi sp, sp, 24 # deallocate local variables temp6, temp5, tmp
     111	  addi a0, x0, 0 # Use 0 return code
     112	  addi a7, x0, 93 # Service command code 93 terminates
     113	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
  rukaml_print_int 11
