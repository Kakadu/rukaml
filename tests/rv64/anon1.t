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
    let u = fresh1 z  in
      let w = print 255  in
        u 1 
  ANF: let revapply x k =
         k x 
       let const0 x =
         0
       let fresh1 z x =
         z const0 
       let z k =
         0
       let main =
         let u = fresh1 z  in
           let w = print 255  in
             u 1 
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
       3	  addi sp, sp, -16 # RA and 1st arg of function k
       4	  sd ra, 8(sp)
       5	  ld t5, 16(sp)
       6	  sd t5, (sp) # access a var "x"
       7	  ld a0, 24(sp)
       8	  li a1, 1
       9	  ld a2, (sp)
      10	  call rukaml_applyN
      11	  ld ra, 8(sp)
      12	  addi sp, sp, 16 # free space for ra and arg 1 of function "k"
      13	  ret # revapply
      14	.globl const0
      15	const0:
      16	  li a0, 0
      17	  ret # const0
      18	.globl fresh1
      19	fresh1:
      20	  addi sp, sp, -16 # RA and 1st arg of function z
      21	  sd ra, 8(sp)
      22	  lla a0, const0
      23	  li a1, 1
      24	  call rukaml_alloc_closure
      25	  sd a0, (sp)
      26	  ld a0, 16(sp)
      27	  li a1, 1
      28	  ld a2, (sp)
      29	  call rukaml_applyN
      30	  ld ra, 8(sp)
      31	  addi sp, sp, 16 # free space for ra and arg 1 of function "z"
      32	  ret # fresh1
      33	.globl z
      34	z:
      35	  li a0, 0
      36	  ret # z
      37	.globl main
      38	main:
      39	  addi sp, sp, -16 # allocate for local variables w, u
      40	  addi sp, sp, -8 #  for func closure
      41	  addi sp, sp, -8 # alloc space for RA register
      42	  lla a0, fresh1
      43	  li a1, 2
      44	  sd ra, (sp)
      45	  call rukaml_alloc_closure
      46	  ld ra, (sp)
      47	  addi sp, sp, 8 # free space of RA register
      48	  sd a0, (sp)
      49	# Allocate args to call fun "fresh1" arguments
      50	  addi sp, sp, -8
      51	  lla a0, z
      52	  li a1, 1
      53	  call rukaml_alloc_closure
      54	  sd a0, (sp)
      55	  addi sp, sp, -8 # alloc space for RA register
      56	  ld a0, 16(sp)
      57	  li a1, 1
      58	  ld a2, 8(sp) # arg 0
      59	  sd ra, (sp)
      60	  call rukaml_applyN
      61	  sd a0, 32(sp)
      62	  ld ra, (sp)
      63	  addi sp, sp, 8 # free space of RA register
      64	  addi sp, sp, 8 # deallocate 1 args
      65	  addi sp, sp, 8 # deallocate closure value
      66	  addi sp, sp, -16
      67	  li a0, 255
      68	  sd a0, (sp)
      69	  sd ra, 8(sp)
      70	  call rukaml_print_int
      71	  ld ra, 8(sp)
      72	  sd a0, 16(sp)
      73	  addi sp, sp, 16
      74	  addi sp, sp, -16 # RA and 1st arg of function u
      75	  sd ra, 8(sp)
      76	  li t0, 1
      77	  sd t0, (sp)
      78	  ld a0, 24(sp)
      79	  li a1, 1
      80	  ld a2, (sp)
      81	  call rukaml_applyN
      82	  ld ra, 8(sp)
      83	  addi sp, sp, 16 # free space for ra and arg 1 of function "u"
      84	  addi sp, sp, 16 # deallocate local variables w, u
      85	  addi a0, x0, 0 # Use 0 return code
      86	  addi a7, x0, 93 # Service command code 93 terminates
      87	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
  $ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./program.exe
  rukaml_print_int 255
