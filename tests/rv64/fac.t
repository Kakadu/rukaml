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
       3	  addi sp, sp, -(8*3) # allocate for local variables p2, p, temp
       4	 # 32(sp), find_exn "n" = 0, last_pos = 3  
       5	  ld t0, 32(sp) # locals = 3
       6	  li t1, 1
       7	  beq t0, t1, lab_8
       8	  sd zero, 16(sp)
       9	  beq zero, zero, lab_9 # Where is unconditional jump?
      10	lab_8:
      11	  li t0, 1 # (* RISC is weird *)
      12	  sd t0, 16(sp) # dest = 16(sp)
      13	  beq zero, zero, lab_9 # not needed?
      14	lab_9:
      15	  ld  t0, 16(sp)
      16	  beq t0, zero, lab_else_10
      17	  li a0, 1
      18	  beq zero, zero, lab_endif_11
      19	lab_else_10:  # temp is 0 
      20	  ld t0, 32(sp)
      21	  addi t0, t0, -1
      22	  sd t0, 8(sp)
      23	  # Allocate args to call fun "fac" arguments
      24	  addi sp, sp, -8 #
      25	  ld t0, 16(sp)  # arg "p"
      26	  sd t0, (sp)
      27	  addi sp, sp, -8
      28	  sd ra, (sp)
      29	  call fac
      30	  ld ra, (sp)
      31	  addi sp, sp, 8
      32	  addi sp, sp, 8*1 # deallocate 1 args
      33	  sd a0, (sp)
      34	  ld t3, 32(sp) #
      35	  ld t4, (sp)
      36	  mulw t5, t3, t4
      37	  addi a0, t5, 0
      38	lab_endif_11:
      39	  addi sp, sp, 8*3 # deallocate local variables p2, p, temp
      40	  ret  # fac
      41	.globl main
      42	main:
      43	  addi sp, sp, -(8*2) # allocate for local variables g, f
      44	  # Allocate args to call fun "fac" arguments
      45	  addi sp, sp, -8 #
      46	  li t0, 4
      47	  sd t0, (sp) # constant
      48	  addi sp, sp, -8
      49	  sd ra, (sp)
      50	  call fac
      51	  ld ra, (sp)
      52	  addi sp, sp, 8
      53	  addi sp, sp, 8*1 # deallocate 1 args
      54	  sd a0, 8(sp)
      55	  lw a0, 8(sp)
      56	  call rukaml_print_int
      57	  sw a0, (sp)
      58	  li a0, 0
      59	  addi sp, sp, 8*2 # deallocate local variables g, f
      60	  addi    a0, x0, 0   # Use 0 return code
      61	  addi    a7, x0, 93  # Service command code 93 terminates
      62	  ecall               # Call linux to terminate the program
$ ls 
$ ls ../../back_rv64

  $ riscv64-linux-gnu-gcc -c -g program.s -o program.o # 2>&1 | head -n5
$ riscv64-linux-gnu-gcc -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc -g program.o ../../back_rv64/rukaml_stdlib.o -o fac.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fac.exe
  H
  rukaml_print_int 24
