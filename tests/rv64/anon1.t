  $ cat anon1.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let fresh_2 x =
    0
  let fresh_1 z x =
    z fresh_2 
  let z k =
    0
  let ret x k =
    k x 
  let main =
    let temp3 = ret 1  in
      let temp4 = fresh_1 z  in
        temp3 temp4 
  ANF: let fresh_2 x =
         0
       let fresh_1 z x =
         z fresh_2 
       let z k =
         0
       let ret x k =
         k x 
       let main =
         let temp3 = ret 1  in
           let temp4 = fresh_1 z  in
             temp3 temp4 
  Location argument "x" in [rbp+0]
  Removing info about args [ x ]
  Location argument "x" in [rbp+1]
  Location argument "z" in [rbp+0]
  Removing info about args [ z, x ]
  Location argument "k" in [rbp+0]
  Removing info about args [ k ]
  Location argument "k" in [rbp+1]
  Location argument "x" in [rbp+0]
  Removing info about args [ x, k ]
  Removing info about args [  ]
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl fresh_2
       2	fresh_2:
       3	  li a0, 0
       4	  ret # fresh_2
       5	.globl fresh_1
       6	fresh_1:
       7	  addi sp, sp, -8 # first arg of a function z
       8	  lla a0, fresh_2
       9	  li a1, 1
      10	  call rukaml_alloc_closure
      11	  sd a0, (sp)
      12	  addi sp, sp, -8 # alloc space for RA register
      13	  ld a0, 16(sp)
      14	  li a1, 1
      15	  ld a2, 8(sp)
      16	  sd ra, (sp)
      17	  call rukaml_applyN
      18	  ld ra, (sp)
      19	  addi sp, sp, 8 # free space of RA register
      20	  addi sp, sp, 8 # free space for args of function "z"
      21	  ret # fresh_1
      22	.globl z
      23	z:
      24	  li a0, 0
      25	  ret # z
      26	.globl ret
      27	ret:
      28	  addi sp, sp, -8 # first arg of a function k
      29	  ld t5, 8(sp)
      30	  sd t5, (sp) # access a var "x"
      31	  addi sp, sp, -8 # alloc space for RA register
      32	  ld a0, 24(sp)
      33	  li a1, 1
      34	  ld a2, 8(sp)
      35	  sd ra, (sp)
      36	  call rukaml_applyN
      37	  ld ra, (sp)
      38	  addi sp, sp, 8 # free space of RA register
      39	  addi sp, sp, 8 # free space for args of function "k"
      40	  ret # ret
      41	.globl main
      42	main:
      43	  addi sp, sp, -16 # allocate for local variables temp4, temp3
      44	  addi sp, sp, -8 #  for func closure
      45	  addi sp, sp, -8 # alloc space for RA register
      46	  lla a0, ret
      47	  li a1, 2
      48	  sd ra, (sp)
      49	  call rukaml_alloc_closure
      50	  ld ra, (sp)
      51	  addi sp, sp, 8 # free space of RA register
      52	  sd a0, (sp)
      53	# Allocate args to call fun "ret" arguments
      54	  addi sp, sp, -8
      55	  li t0, 1
      56	  sd t0, (sp) # constant
      57	  addi sp, sp, -8 # alloc space for RA register
      58	  ld a0, 16(sp)
      59	  li a1, 1
      60	  ld a2, 8(sp) # arg 0
      61	  sd ra, (sp)
      62	  call rukaml_applyN
      63	  sd a0, 32(sp)
      64	  ld ra, (sp)
      65	  addi sp, sp, 8 # free space of RA register
      66	  addi sp, sp, 8 # deallocate 1 args
      67	  addi sp, sp, 8 # deallocate closure value
      68	  addi sp, sp, -8 #  for func closure
      69	  addi sp, sp, -8 # alloc space for RA register
      70	  lla a0, fresh_1
      71	  li a1, 2
      72	  sd ra, (sp)
      73	  call rukaml_alloc_closure
      74	  ld ra, (sp)
      75	  addi sp, sp, 8 # free space of RA register
      76	  sd a0, (sp)
      77	# Allocate args to call fun "fresh_1" arguments
      78	  addi sp, sp, -8
      79	  lla a0, z
      80	  li a1, 1
      81	  call rukaml_alloc_closure
      82	  sd a0, (sp)
      83	  addi sp, sp, -8 # alloc space for RA register
      84	  ld a0, 16(sp)
      85	  li a1, 1
      86	  ld a2, 8(sp) # arg 0
      87	  sd ra, (sp)
      88	  call rukaml_applyN
      89	  sd a0, 24(sp)
      90	  ld ra, (sp)
      91	  addi sp, sp, 8 # free space of RA register
      92	  addi sp, sp, 8 # deallocate 1 args
      93	  addi sp, sp, 8 # deallocate closure value
      94	  addi sp, sp, -8 # first arg of a function temp3
      95	  ld t5, 8(sp)
      96	  sd t5, (sp) # access a var "temp4"
      97	  addi sp, sp, -8 # alloc space for RA register
      98	  ld a0, 24(sp)
      99	  li a1, 1
     100	  ld a2, 8(sp)
     101	  sd ra, (sp)
     102	  call rukaml_applyN
     103	  ld ra, (sp)
     104	  addi sp, sp, 8 # free space of RA register
     105	  addi sp, sp, 8 # free space for args of function "temp3"
     106	  addi sp, sp, 16 # deallocate local variables temp4, temp3
     107	  addi a0, x0, 0 # Use 0 return code
     108	  addi a7, x0, 93 # Service command code 93 terminates
     109	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
  Segmentation fault (core dumped)
  [139]
