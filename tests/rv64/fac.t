  $ cat fac.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let fac n =
    let temp = (n = 1) in
      (if temp
      then 1
      else let p = (n - 1) in
             let p2 = fac p  in
               (n * p2))
  let main =
    let f = fac 4  in
      let g = print f  in
        0
  ANF: let fac n =
         let temp = (n = 1) in
           (if temp
           then 1
           else let p = (n - 1) in
                  let p2 = fac p  in
                    (n * p2))
       let main =
         let f = fac 4  in
           let g = print f  in
             0
  Location argument "n" in [rbp+0]
  Removing info about args [ n ]
  Removing info about args [  ]
 
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl fac
       2	fac:
       3	  addi sp, sp, -24 # allocate for local variables p2, p, temp
       4	# 24(sp), find_exn "n" = 0, last_pos = 3
       5	  ld t0, 24(sp) # locals = 3
       6	  li t1, 1
       7	  beq t0, t1, lab_8
       8	  sd zero, 16(sp)
       9	  beq zero, zero, lab_9
      10	lab_8:
      11	  li t0, 1
      12	  sd t0, 16(sp) # dest = 16(sp)
      13	  beq zero, zero, lab_9
      14	lab_9:
      15	  ld t0, 16(sp)
      16	  beq t0, zero, lab_else_10
      17	  li a0, 1
      18	  beq zero, zero, lab_endif_11
      19	lab_else_10: # temp is 0
      20	  ld t0, 24(sp)
      21	  addi t0, t0, -1
      22	  sd t0, 8(sp)
      23	  addi sp, sp, -8 # alloc space for RA register
      24	  sd ra, (sp)
      25	# Allocate args to call fun "fac" arguments
      26	  addi sp, sp, -8
      27	  ld t0, 24(sp) # arg "p"
      28	  sd t0, (sp)
      29	  call fac
      30	  addi sp, sp, 8 # deallocate 1 args
      31	  ld ra, (sp)
      32	  addi sp, sp, 8 # free space of RA register
      33	  sd a0, (sp)
      34	# n is stored in 0
      35	# p2 is stored in 3
      36	# last_pos = 3
      37	  ld t3, 24(sp)
      38	  ld t4, (sp)
      39	  mulw t5, t3, t4
      40	  addi a0, t5, 0
      41	lab_endif_11:
      42	  addi sp, sp, 24 # deallocate local variables p2, p, temp
      43	  ret # fac
      44	.globl main
      45	main:
      46	  addi sp, sp, -16 # allocate for local variables g, f
      47	  addi sp, sp, -8 # alloc space for RA register
      48	  sd ra, (sp)
      49	# Allocate args to call fun "fac" arguments
      50	  addi sp, sp, -8
      51	  li t0, 4
      52	  sd t0, (sp) # constant
      53	  call fac
      54	  addi sp, sp, 8 # deallocate 1 args
      55	  ld ra, (sp)
      56	  addi sp, sp, 8 # free space of RA register
      57	  sd a0, 8(sp)
      58	  addi sp, sp, -8 # alloc space for RA register
      59	  ld a0, 16(sp)
      60	  sd ra, (sp)
      61	  call rukaml_print_int
      62	  sd a0, 8(sp)
      63	  ld ra, (sp)
      64	  addi sp, sp, 8 # free space of RA register
      65	  li a0, 0
      66	  addi sp, sp, 16 # deallocate local variables g, f
      67	  addi a0, x0, 0 # Use 0 return code
      68	  addi a7, x0, 93 # Service command code 93 terminates
      69	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fac.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fac.exe
  rukaml_print_int 24
