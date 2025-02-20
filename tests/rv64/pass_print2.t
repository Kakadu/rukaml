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
      39	  addi sp, sp, -16 #  RA + closure
      40	  sd ra, 8(sp)
      41	  lla a0, foo
      42	  li a1, 2
      43	  call rukaml_alloc_closure
      44	  sd a0, (sp)
      45	# Allocate args to call fun "foo" arguments
      46	  addi sp, sp, -8
      47	  lla a0, rukaml_print_int
      48	  li a1, 1
      49	  call rukaml_alloc_closure
      50	  sd a0, (sp)
      51	  ld a0, 8(sp)
      52	  li a1, 1
      53	  ld a2, (sp) # arg 0
      54	  call rukaml_applyN
      55	  sd a0, 40(sp)
      56	  addi sp, sp, 8 # deallocate 1 args
      57	  ld ra, 8(sp)
      58	  addi sp, sp, 16 # deallocate RA + closure
      59	  addi sp, sp, -16 #  RA + closure
      60	  sd ra, 8(sp)
      61	  lla a0, addk
      62	  li a1, 3
      63	  call rukaml_alloc_closure
      64	  sd a0, (sp)
      65	# Allocate args to call fun "addk" arguments
      66	  addi sp, sp, -8
      67	  li t0, 1
      68	  sd t0, (sp) # constant
      69	  ld a0, 8(sp)
      70	  li a1, 1
      71	  ld a2, (sp) # arg 0
      72	  call rukaml_applyN
      73	  sd a0, 32(sp)
      74	  addi sp, sp, 8 # deallocate 1 args
      75	  ld ra, 8(sp)
      76	  addi sp, sp, 16 # deallocate RA + closure
      77	  addi sp, sp, -16 # RA and 1st arg of function temp5
      78	  sd ra, 8(sp)
      79	  li t0, 10
      80	  sd t0, (sp)
      81	  ld a0, 24(sp)
      82	  li a1, 1
      83	  ld a2, (sp)
      84	  call rukaml_applyN
      85	  ld ra, 8(sp)
      86	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp5"
      87	  sd a0, (sp)
      88	  addi sp, sp, -16 # RA and 1st arg of function temp6
      89	  sd ra, 8(sp)
      90	  ld t5, 32(sp)
      91	  sd t5, (sp) # access a var "tmp"
      92	  ld a0, 16(sp)
      93	  li a1, 1
      94	  ld a2, (sp)
      95	  call rukaml_applyN
      96	  ld ra, 8(sp)
      97	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp6"
      98	  addi sp, sp, 24 # deallocate local variables temp6, temp5, tmp
      99	  addi a0, x0, 0 # Use 0 return code
     100	  addi a7, x0, 93 # Service command code 93 terminates
     101	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
  rukaml_print_int 11
