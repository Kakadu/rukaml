  $ ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf fac_acc.ml
  After ANF transformation.
  let rec fac acc n =
    (if (n < 2)
    then acc
    else let n1 = (n - 1) in
           let p1 = (acc * n) in
             fac p1 n1)
  let main =
    let f = fac 1 4 in
      let g = print f  in
        0

  $ cat program.s | grep -v 'section .note.GNU-stack'
  
  .globl fac
  fac:
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals p1, n1
    sd ra, 16(sp)
    ld t0, 40(sp) # access n
    li t1, 2
    blt t0, t1, lab_then_10
    ld t0, 40(sp)
    addi t0, t0, -1
    sd t0, 8(sp)
  # acc is stored in 0
  # n is stored in -1
  # last_pos = 4
    ld t3, 32(sp)
    ld t4, 40(sp)
    mulw t5, t3, t4
    sd t5, (sp)
  # Allocate args to call fun "fac" with args
    addi sp, sp, -16 # last_pos = 6
    ld t0, 16(sp) # arg "p1"
    sd t0, (sp)
    ld t0, 24(sp) # arg "n1"
    sd t0, 8(sp)
    call fac
    addi sp, sp, 16 # deallocate 2 args
    mv a0, a0
    beq zero, zero, lab_fin_11
  lab_then_10:
    ld t5, 32(sp)
    addi a0, t5, 0
  lab_fin_11:
    ld ra, 16(sp)
    addi sp, sp, 32 # DEallocate for Pad, RA and 2 locals variables p1, n1
    ret # fac
  
  .globl main
  main:
  # this is main
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals g, f
    sd ra, 16(sp)
  # Allocate args to call fun "fac" with args
    addi sp, sp, -16 # last_pos = 6
    li t0, 1
    sd t0, (sp) # constant
    li t0, 4
    sd t0, 8(sp) # constant
    call fac
    addi sp, sp, 16 # deallocate 2 args
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
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fac_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fac_acc.exe
  rukaml_print_int 24
