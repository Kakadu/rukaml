.globl add
add:
# a is stored in 0
# b is stored in -1
# last_pos = 0
  ld t3, (sp)
  ld t4, 8(sp)
  add  t5, t3, t4
  addi a0, t5, 0
  ret # add
.globl main
main:
  addi sp, sp, -24 # allocate for local variables y, x, temp2
  addi sp, sp, -16 #  RA + closure
  sd ra, 8(sp)
  lla a0, add
  li a1, 2
  call rukaml_alloc_closure
  sd a0, (sp)
# Allocate args to call fun "add" arguments
  addi sp, sp, -8
  li t0, 1
  sd t0, (sp) # constant
  ld a0, 8(sp)
  li a1, 1
  ld a2, 8(sp) # arg 0
  call rukaml_applyN
  sd a0, 40(sp)
  addi sp, sp, 8 # deallocate 1 args
  ld ra, 8(sp)
  addi sp, sp, 16 # deallocate RA + closure
  addi sp, sp, -16 # RA and 1st arg of function temp2
  sd ra, 8(sp)
  li t0, 10
  sd t0, (sp)
  ld a0, 32(sp)
  li a1, 1
  ld a2, (sp)
  call rukaml_applyN
  ld ra, 8(sp)
  addi sp, sp, 16 # free space for ra and arg 1 of function "temp2"
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
  addi sp, sp, 24 # deallocate local variables y, x, temp2
  addi a0, x0, 0 # Use 0 return code
  addi a7, x0, 93 # Service command code 93 terminates
  ecall # Call linux to terminate the program
