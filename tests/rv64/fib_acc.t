  $ ../../back/rv64/RV64_compiler.exe -o program.s --no-start -danf fib_acc.ml
  After ANF transformation.
  let rec fib_acc a b n =
    (if (n = 1)
    then b
    else let n1 = (n - 1) in
           let ab = (a + b) in
             fib_acc b ab n1)
  let main =
    let f = fib_acc 0 1 4 in
      let g = print f  in
        0
  $ cat program.s | grep -v 'section .note.GNU-stack'
  
  .globl fib_acc
  fib_acc:
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals ab, n1
    sd ra, 16(sp)
    ld t0, 48(sp) # access n
    li t1, 1
    beq t0, t1, lab_then_12
    ld t0, 48(sp)
    addi t0, t0, -1
    sd t0, 8(sp)
  # a is stored in 0
  # b is stored in -1
  # last_pos = 4
    ld t3, 32(sp)
    ld t4, 40(sp)
    add  t5, t3, t4
    sd t5, (sp)
  # Allocate args to call fun "fib_acc" with args
    addi sp, sp, -32 # last_pos = 8
    ld t0, 72(sp) # arg "b"
    sd t0, (sp)
    ld t0, 32(sp) # arg "ab"
    sd t0, 8(sp)
    ld t0, 40(sp) # arg "n1"
    sd t0, 16(sp)
    call fib_acc
    addi sp, sp, 32 # deallocate 3 args
    mv a0, a0
    beq zero, zero, lab_fin_13
  lab_then_12:
    ld t5, 40(sp)
    addi a0, t5, 0
  lab_fin_13:
    ld ra, 16(sp)
    addi sp, sp, 32 # DEallocate for Pad, RA and 2 locals variables ab, n1
    ret # fib_acc
  
  .globl main
  main:
  # this is main
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals g, f
    sd ra, 16(sp)
  # Allocate args to call fun "fib_acc" with args
    addi sp, sp, -32 # last_pos = 8
    li t0, 0
    sd t0, (sp) # constant
    li t0, 1
    sd t0, 8(sp) # constant
    li t0, 4
    sd t0, 16(sp) # constant
    call fib_acc
    addi sp, sp, 32 # deallocate 3 args
    sd a0, 8(sp)
    addi sp, sp, -16
    sd ra, 8(sp)
    ld t0, 24(sp)
    sd t0, (sp)
    call rukaml_print_int
    ld ra, 8(sp)
    sd a0, 16(sp)
    addi sp, sp, 16
    li a0, 0
    ld ra, 16(sp)
    addi sp, sp, 32 # DEallocate for Pad, RA and 2 locals variables g, f
  #  fin
    addi a0, x0, 0 # Use 0 return code
    addi a7, x0, 93 # Service command code 93 terminates
    ecall # Call linux to terminate the program
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back/rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back/rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fib_acc.exe
  rukaml_print_int 3
