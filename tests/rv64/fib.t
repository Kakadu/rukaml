  $ ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf fib.ml
  After ANF transformation.
  let rec fib n =
    (if (n < 2)
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
  $ cat program.s | grep -v 'section .note.GNU-stack'
  
  .globl fib
  fib:
    addi sp, sp, -48 # allocate for Pad, RA, and 4 locals temp5, temp3, p2, p1
    sd ra, 32(sp)
    ld t0, 48(sp) # access n
    li t1, 2
    blt t0, t1, lab_then_10
    ld t0, 48(sp)
    addi t0, t0, -1
    sd t0, 8(sp)
  # Allocate args to call fun "fib" with args
    addi sp, sp, -16 # last_pos = 8
    ld t0, 24(sp) # arg "temp3"
    sd t0, (sp)
    call fib
    addi sp, sp, 16 # deallocate 1 args
    sd a0, 24(sp)
    ld t5, 48(sp)
    addi t5, t5, -2
    sd t5, (sp)
  # Allocate args to call fun "fib" with args
    addi sp, sp, -16 # last_pos = 8
    ld t0, 16(sp) # arg "temp5"
    sd t0, (sp)
    call fib
    addi sp, sp, 16 # deallocate 1 args
    sd a0, 16(sp)
  # p1 is stored in 3
  # p2 is stored in 4
  # last_pos = 6
    ld t3, 24(sp)
    ld t4, 16(sp)
    add  t5, t3, t4
    addi a0, t5, 0
    beq zero, zero, lab_fin_11
  lab_then_10:
    ld t5, 48(sp)
    addi a0, t5, 0
  lab_fin_11:
    ld ra, 32(sp)
    addi sp, sp, 48 # DEallocate for Pad, RA and 4 locals variables temp5, temp3, p2, p1
    ret # fib
  
  .globl main
  main:
  # this is main
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals g, f
    sd ra, 16(sp)
  # Allocate args to call fun "fib" with args
    addi sp, sp, -16 # last_pos = 6
    li t0, 6
    sd t0, (sp) # constant
    call fib
    addi sp, sp, 16 # deallocate 1 args
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
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o && ./program.exe
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fib.exe
  rukaml_print_int 8
