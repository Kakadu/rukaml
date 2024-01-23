  $ cat fib.ml | ../../back_rv64/RV64_compiler.exe -o program.s --no-start -vamd64 -
  After ANF transformation.
  let fib n =
    let temp = (n < 2) in
      (if temp
      then n
      else let temp3 = (n - 1) in
             let p1 = fib temp3  in
               let temp5 = (n - 2) in
                 let p2 = fib temp5  in
                   (p1 + p2))
  let main =
    let f = fib 6  in
      let g = print f  in
        0
  ANF: let fib n =
         let temp = (n < 2) in
           (if temp
           then n
           else let temp3 = (n - 1) in
                  let p1 = fib temp3  in
                    let temp5 = (n - 2) in
                      let p2 = fib temp5  in
                        (p1 + p2))
       let main =
         let f = fib 6  in
           let g = print f  in
             0
  Location argument "n" in [rbp+0]
  Removing info about args [ n ]
  Removing info about args [  ]
  $ cat program.s | grep -v 'section .note.GNU-stack' | nl -ba
       1	.globl fib
       2	fib:
       3	  addi sp, sp, -40 # allocate for local variables p2, temp5, p1, temp3, temp
       4	# 40(sp), find_exn "n" = 0, last_pos = 5
       5	  ld t0, 40(sp) # locals = 5
       6	  li t1, 2
       7	  blt t0, t1, lab_10
       8	  sd zero, 32(sp)
       9	  beq zero, zero, lab_11
      10	lab_10:
      11	  li t0, 1
      12	  sd t0, 32(sp) # dest = 32(sp)
      13	  beq zero, zero, lab_11
      14	lab_11:
      15	  ld t0, 32(sp)
      16	  beq t0, zero, lab_else_12
      17	  ld t5, 40(sp)
      18	  addi a0, t5, 0
      19	  beq zero, zero, lab_endif_13
      20	lab_else_12: # temp is 0
      21	  ld t0, 40(sp)
      22	  addi t0, t0, -1
      23	  sd t0, 24(sp)
      24	  addi sp, sp, -8 # alloc space for RA register
      25	# Allocate args to call fun "fib" arguments
      26	  addi sp, sp, -8
      27	  ld t0, 40(sp) # arg "temp3"
      28	  sd t0, (sp)
      29	  sd ra, 8(sp)
      30	  call fib
      31	  addi sp, sp, 8 # deallocate 1 args
      32	  ld ra, (sp)
      33	  addi sp, sp, 8 # free space of RA register
      34	  sd a0, 16(sp)
      35	  ld t5, 40(sp)
      36	  addi t5, t5, -2
      37	  sd t5, 8(sp)
      38	  addi sp, sp, -8 # alloc space for RA register
      39	# Allocate args to call fun "fib" arguments
      40	  addi sp, sp, -8
      41	  ld t0, 24(sp) # arg "temp5"
      42	  sd t0, (sp)
      43	  sd ra, 8(sp)
      44	  call fib
      45	  addi sp, sp, 8 # deallocate 1 args
      46	  ld ra, (sp)
      47	  addi sp, sp, 8 # free space of RA register
      48	  sd a0, (sp)
      49	# p1 is stored in 3
      50	# p2 is stored in 5
      51	# last_pos = 5
      52	  ld t3, 16(sp)
      53	  ld t4, (sp)
      54	  add  t5, t3, t4
      55	  addi a0, t5, 0
      56	lab_endif_13:
      57	  addi sp, sp, 40 # deallocate local variables p2, temp5, p1, temp3, temp
      58	  ret # fib
      59	.globl main
      60	main:
      61	  addi sp, sp, -16 # allocate for local variables g, f
      62	  addi sp, sp, -8 # alloc space for RA register
      63	# Allocate args to call fun "fib" arguments
      64	  addi sp, sp, -8
      65	  li t0, 6
      66	  sd t0, (sp) # constant
      67	  sd ra, 8(sp)
      68	  call fib
      69	  addi sp, sp, 8 # deallocate 1 args
      70	  ld ra, (sp)
      71	  addi sp, sp, 8 # free space of RA register
      72	  sd a0, 8(sp)
      73	  addi sp, sp, -8 # alloc space for RA register
      74	  ld a0, 16(sp)
      75	  sd ra, (sp)
      76	  call rukaml_print_int
      77	  sd a0, 8(sp)
      78	  ld ra, (sp)
      79	  addi sp, sp, 8 # free space of RA register
      80	  li a0, 0
      81	  addi sp, sp, 16 # deallocate local variables g, f
      82	  addi a0, x0, 0 # Use 0 return code
      83	  addi a7, x0, 93 # Service command code 93 terminates
      84	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o  
$ riscv64-linux-gnu-gcc -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib.exe 
  8
  rukaml_print_int 8
