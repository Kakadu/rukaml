  $ ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf fac.ml
  After ANF transformation.
  let rec fac n =
    (if (n = 1)
    then 1
    else let p = (n - 1) in
           let p2 = fac p  in
             (n * p2))
  let main =
    let f = fac 5  in
      let g = print f  in
        0

  $ cat program.s | grep -v 'section .note.GNU-stack'
  
  .globl fac
  fac:
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals p2, p
    sd ra, 16(sp)
    ld t0, 32(sp) # access n
    li t1, 1
    beq t0, t1, lab_then_8
    ld t0, 32(sp)
    addi t0, t0, -1
    sd t0, 8(sp)
  # Allocate args to call fun "fac" with args
    addi sp, sp, -16 # last_pos = 6
    ld t0, 24(sp) # arg "p"
    sd t0, (sp)
    call fac
    addi sp, sp, 16 # deallocate 1 args
    sd a0, (sp)
  # n is stored in 0
  # p2 is stored in 4
  # last_pos = 4
    ld t3, 32(sp)
    ld t4, (sp)
    mulw t5, t3, t4
    addi a0, t5, 0
    beq zero, zero, lab_fin_9
  lab_then_8:
    li a0, 1
  lab_fin_9:
    ld ra, 16(sp)
    addi sp, sp, 32 # DEallocate for Pad, RA and 2 locals variables p2, p
    ret # fac
  
  .globl main
  main:
  # this is main
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals g, f
    sd ra, 16(sp)
  # Allocate args to call fun "fac" with args
    addi sp, sp, -16 # last_pos = 6
    li t0, 5
    sd t0, (sp) # constant
    call fac
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
  $ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o # 2>&1 | head -n5
  $ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fac.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./fac.exe
  rukaml_print_int 120
