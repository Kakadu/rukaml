  $ cat anon1.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let revapply x k =
    k x 
  let const0 x =
    0
  let fresh1 z x =
    z const0 
  let z k =
    0
  let main =
    let w = print 127  in
      let u = fresh1 z  in
        let w = print 255  in
          let temp6 = revapply 1  in
            temp6 u 
  ANF: let revapply x k =
         k x 
       let const0 x =
         0
       let fresh1 z x =
         z const0 
       let z k =
         0
       let main =
         let w = print 127  in
           let u = fresh1 z  in
             let w = print 255  in
               let temp6 = revapply 1  in
                 temp6 u 
  Location argument "k" in [rbp+1]
  Location argument "x" in [rbp+0]
  Removing info about args [ x, k ]
  Location argument "x" in [rbp+0]
  Removing info about args [ x ]
  Location argument "x" in [rbp+1]
  Location argument "z" in [rbp+0]
  Removing info about args [ z, x ]
  Location argument "k" in [rbp+0]
  Removing info about args [ k ]
  Removing info about args [  ]
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl revapply
       2	revapply:
       3	  addi sp, sp, -8 # first arg of a function k
       4	  ld t5, 8(sp)
       5	  sd t5, (sp) # access a var "x"
       6	  addi sp, sp, -8 # alloc space for RA register
       7	  ld a0, 24(sp)
       8	  li a1, 1
       9	  ld a2, 8(sp)
      10	  sd ra, (sp)
      11	  call rukaml_applyN
      12	  ld ra, (sp)
      13	  addi sp, sp, 8 # free space of RA register
      14	  addi sp, sp, 8 # free space for args of function "k"
      15	  ret # revapply
      16	.globl const0
      17	const0:
      18	  li a0, 0
      19	  ret # const0
      20	.globl fresh1
      21	fresh1:
      22	  addi sp, sp, -8 # first arg of a function z
      23	  lla a0, const0
      24	  li a1, 1
      25	  call rukaml_alloc_closure
      26	  sd a0, (sp)
      27	  addi sp, sp, -8 # alloc space for RA register
      28	  ld a0, 16(sp)
      29	  li a1, 1
      30	  ld a2, 8(sp)
      31	  sd ra, (sp)
      32	  call rukaml_applyN
      33	  ld ra, (sp)
      34	  addi sp, sp, 8 # free space of RA register
      35	  addi sp, sp, 8 # free space for args of function "z"
      36	  ret # fresh1
      37	.globl z
      38	z:
      39	  li a0, 0
      40	  ret # z
      41	.globl main
      42	main:
      43	  addi sp, sp, -32 # allocate for local variables temp6, w, u, w
      44	  addi sp, sp, -8 # alloc space for RA register
      45	  addi sp, sp, -8
      46	  li a0, 127
      47	  sd a0, (sp)
      48	  sd ra, (sp)
      49	  call rukaml_print_int
      50	  sd a0, 32(sp)
      51	  ld ra, (sp)
      52	  addi sp, sp, 8 # free space of RA register
      53	  addi sp, sp, -8 #  for func closure
      54	  addi sp, sp, -8 # alloc space for RA register
      55	  lla a0, fresh1
      56	  li a1, 2
      57	  sd ra, (sp)
      58	  call rukaml_alloc_closure
      59	  ld ra, (sp)
      60	  addi sp, sp, 8 # free space of RA register
      61	  sd a0, (sp)
      62	# Allocate args to call fun "fresh1" arguments
      63	  addi sp, sp, -8
      64	  lla a0, z
      65	  li a1, 1
      66	  call rukaml_alloc_closure
      67	  sd a0, (sp)
      68	  addi sp, sp, -8 # alloc space for RA register
      69	  ld a0, 16(sp)
      70	  li a1, 1
      71	  ld a2, 8(sp) # arg 0
      72	  sd ra, (sp)
      73	  call rukaml_applyN
      74	  sd a0, 40(sp)
      75	  ld ra, (sp)
      76	  addi sp, sp, 8 # free space of RA register
      77	  addi sp, sp, 8 # deallocate 1 args
      78	  addi sp, sp, 8 # deallocate closure value
      79	  addi sp, sp, -8 # alloc space for RA register
      80	  addi sp, sp, -8
      81	  li a0, 255
      82	  sd a0, (sp)
      83	  sd ra, (sp)
      84	  call rukaml_print_int
      85	  sd a0, 16(sp)
      86	  ld ra, (sp)
      87	  addi sp, sp, 8 # free space of RA register
      88	  addi sp, sp, -8 #  for func closure
      89	  addi sp, sp, -8 # alloc space for RA register
      90	  lla a0, revapply
      91	  li a1, 2
      92	  sd ra, (sp)
      93	  call rukaml_alloc_closure
      94	  ld ra, (sp)
      95	  addi sp, sp, 8 # free space of RA register
      96	  sd a0, (sp)
      97	# Allocate args to call fun "revapply" arguments
      98	  addi sp, sp, -8
      99	  li t0, 1
     100	  sd t0, (sp) # constant
     101	  addi sp, sp, -8 # alloc space for RA register
     102	  ld a0, 16(sp)
     103	  li a1, 1
     104	  ld a2, 8(sp) # arg 0
     105	  sd ra, (sp)
     106	  call rukaml_applyN
     107	  sd a0, 24(sp)
     108	  ld ra, (sp)
     109	  addi sp, sp, 8 # free space of RA register
     110	  addi sp, sp, 8 # deallocate 1 args
     111	  addi sp, sp, 8 # deallocate closure value
     112	  addi sp, sp, -8 # first arg of a function temp6
     113	  ld t5, 24(sp)
     114	  sd t5, (sp) # access a var "u"
     115	  addi sp, sp, -8 # alloc space for RA register
     116	  ld a0, 16(sp)
     117	  li a1, 1
     118	  ld a2, 8(sp)
     119	  sd ra, (sp)
     120	  call rukaml_applyN
     121	  ld ra, (sp)
     122	  addi sp, sp, 8 # free space of RA register
     123	  addi sp, sp, 8 # free space for args of function "temp6"
     124	  addi sp, sp, 32 # deallocate local variables temp6, w, u, w
     125	  addi a0, x0, 0 # Use 0 return code
     126	  addi a7, x0, 93 # Service command code 93 terminates
     127	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
  rukaml_print_int 1047128798
  rukaml_print_int 1431665272
  Segmentation fault (core dumped)
  [139]
