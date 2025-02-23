  $ ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf anon1.ml
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
    let w = print 255  in
      fresh1 z 1
  $ cat program.s | grep -v 'section .note.GNU-stack'
  
  .globl revapply
  revapply:
    addi sp, sp, -16 # allocate for Pad, RA, and 0 locals 
    sd ra, (sp)
    addi sp, sp, -16 # pad and 1st arg of function k
    ld t5, 32(sp)
    sd t5, (sp) # access a var "x"
    ld a0, 40(sp)
    li a1, 1
    ld a2, (sp)
    call rukaml_applyN
    addi sp, sp, 16 # DEalloc for pad and arg 1 of function "k"
    ld ra, (sp)
    addi sp, sp, 16 # DEallocate for Pad, RA and 0 locals variables 
    ret # revapply
  
  .globl const0
  const0:
    addi sp, sp, -16 # allocate for Pad, RA, and 0 locals 
    sd ra, (sp)
    li a0, 0
    ld ra, (sp)
    addi sp, sp, 16 # DEallocate for Pad, RA and 0 locals variables 
    ret # const0
  
  .globl fresh1
  fresh1:
    addi sp, sp, -16 # allocate for Pad, RA, and 0 locals 
    sd ra, (sp)
    addi sp, sp, -16 # pad and 1st arg of function z
    lla a0, const0
    li a1, 1
    call rukaml_alloc_closure
    sd a0, (sp)
    ld a0, 32(sp)
    li a1, 1
    ld a2, (sp)
    call rukaml_applyN
    addi sp, sp, 16 # DEalloc for pad and arg 1 of function "z"
    ld ra, (sp)
    addi sp, sp, 16 # DEallocate for Pad, RA and 0 locals variables 
    ret # fresh1
  
  .globl z
  z:
    addi sp, sp, -16 # allocate for Pad, RA, and 0 locals 
    sd ra, (sp)
    li a0, 0
    ld ra, (sp)
    addi sp, sp, 16 # DEallocate for Pad, RA and 0 locals variables 
    ret # z
  
  .globl main
  main:
  # this is main
    addi sp, sp, -16 # allocate for RA, and 1 locals w
    sd ra, 8(sp)
    addi sp, sp, -16
    li a0, 255
    sd a0, (sp)
    sd ra, 8(sp)
    call rukaml_print_int
    ld ra, 8(sp)
    sd a0, 16(sp)
    addi sp, sp, 16
  # Allocate args to call fun "fresh1" with args
    addi sp, sp, -16 # last_pos = 4
    lla a0, z
    li a1, 1
    call rukaml_alloc_closure
    sd a0, (sp)
    li t0, 1
    sd t0, 8(sp) # constant
    call fresh1
    addi sp, sp, 16 # deallocate 2 args
    mv a0, a0
    ld ra, 8(sp)
    addi sp, sp, 16 # DEallocate for RA, and 1 locals w
  #  fin
    addi a0, x0, 0 # Use 0 return code
    addi a7, x0, 93 # Service command code 93 terminates
    ecall # Call linux to terminate the program
$ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back_rv64/rukaml_stdlib.o program.o
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back_rv64/rukaml_stdlib.o -o fib.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./anon1.exe
  rukaml_print_int 255
