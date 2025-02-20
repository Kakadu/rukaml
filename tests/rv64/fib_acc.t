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
      31	  addi sp, sp, -16 #  RA + closure
      32	  sd ra, 8(sp)
      33	  lla a0, fib_acc
      34	  li a1, 3
      35	  call rukaml_alloc_closure
      36	  sd a0, (sp)
      37	# Allocate args to call fun "fib_acc" arguments
      38	  addi sp, sp, -8
      39	  ld t0, 72(sp) # arg "b"
      40	  sd t0, (sp)
      41	  ld a0, 8(sp)
      42	  li a1, 1
      43	  ld a2, (sp) # arg 0
      44	  call rukaml_applyN
      45	  sd a0, 32(sp)
      46	  addi sp, sp, 8 # deallocate 1 args
      47	  ld ra, 8(sp)
      48	  addi sp, sp, 16 # deallocate RA + closure
      49	  addi sp, sp, -16 # RA and 1st arg of function temp5
      50	  sd ra, 8(sp)
      51	  ld t5, 32(sp)
      52	  sd t5, (sp) # access a var "ab"
      53	  ld a0, 24(sp)
      54	  li a1, 1
      55	  ld a2, (sp)
      56	  call rukaml_applyN
      57	  ld ra, 8(sp)
      58	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp5"
      59	  sd a0, (sp)
      60	  addi sp, sp, -16 # RA and 1st arg of function temp6
      61	  sd ra, 8(sp)
      62	  ld t5, 40(sp)
      63	  sd t5, (sp) # access a var "n1"
      64	  ld a0, 16(sp)
      65	  li a1, 1
      66	  ld a2, (sp)
      67	  call rukaml_applyN
      68	  ld ra, 8(sp)
      69	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp6"
      70	lab_endif_15:
      71	  addi sp, sp, 40 # deallocate local variables temp6, temp5, ab, n1, temp1
      72	  ret # fib_acc
      73	.globl main
      74	main:
      75	  addi sp, sp, -32 # allocate for local variables g, f, temp9, temp8
      76	  addi sp, sp, -16 #  RA + closure
      77	  sd ra, 8(sp)
      78	  lla a0, fib_acc
      79	  li a1, 3
      80	  call rukaml_alloc_closure
      81	  sd a0, (sp)
      82	# Allocate args to call fun "fib_acc" arguments
      83	  addi sp, sp, -8
      84	  li t0, 0
      85	  sd t0, (sp) # constant
      86	  ld a0, 8(sp)
      87	  li a1, 1
      88	  ld a2, (sp) # arg 0
      89	  call rukaml_applyN
      90	  sd a0, 48(sp)
      91	  addi sp, sp, 8 # deallocate 1 args
      92	  ld ra, 8(sp)
      93	  addi sp, sp, 16 # deallocate RA + closure
      94	  addi sp, sp, -16 # RA and 1st arg of function temp8
      95	  sd ra, 8(sp)
      96	  li t0, 1
      97	  sd t0, (sp)
      98	  ld a0, 40(sp)
      99	  li a1, 1
     100	  ld a2, (sp)
     101	  call rukaml_applyN
     102	  ld ra, 8(sp)
     103	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp8"
     104	  sd a0, 16(sp)
     105	  addi sp, sp, -16 # RA and 1st arg of function temp9
     106	  sd ra, 8(sp)
     107	  li t0, 4
     108	  sd t0, (sp)
     109	  ld a0, 32(sp)
     110	  li a1, 1
     111	  ld a2, (sp)
     112	  call rukaml_applyN
     113	  ld ra, 8(sp)
     114	  addi sp, sp, 16 # free space for ra and arg 1 of function "temp9"
     115	  sd a0, 8(sp)
     116	  addi sp, sp, -16
     117	  sd ra, 8(sp)
     118	  ld t0, 24(sp)
     119	  sd t0, (sp)
     120	  call rukaml_print_int
     121	  ld ra, 8(sp)
     122	  sd a0, 16(sp)
     123	  addi sp, sp, 16
     124	  li a0, 0
     125	  addi sp, sp, 32 # deallocate local variables g, f, temp9, temp8
     126	  addi a0, x0, 0 # Use 0 return code
     127	  addi a7, x0, 93 # Service command code 93 terminates
     128	  ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fib_acc.exe
  rukaml_print_int 3
