.globl add
add:
  addi sp, sp, -(8*2) # allocate for local variables v2, u
  lw a0, 24(sp)
  call rukaml_print_int
  sw a0, 8(sp)
  lw a0, 32(sp)
  call rukaml_print_int
  sw a0, (sp)
  ld t3, 24(sp) #
  ld t4, 32(sp)
  add  t5, t3, t4
  addi a0, t5, 0
  addi sp, sp, 8*2 # deallocate local variables v2, u
  ret  # add
.globl main
main:
  addi sp, sp, -(8*3) # allocate for local variables g, f, temp4
  ld a0, add
  addi a1, zero, 2
  call rukaml_alloc_closure # a0 stores closure representation
  # Allocate args to call fun "add" arguments
  addi sp, sp, -8 #
  li t0, 1
  sd t0, (sp) # constant
  li a1, 1
  ld a1, +0*8(sp)
  addi sp, sp, -8
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8
  addi sp, sp, 8*1 # deallocate 1 args
  addi sp, sp, 8*1 # deallocate args of rukaml_applyN
  sd a0, 16(sp)
  addi sp, sp, -8 #first arg of a function temp4
  li t0, 10
  sw t0, (sp)
  ld a0, 24(sp)
  addi a1, zero, 1
  ld a2, (sp)
  call rukaml_applyN
  addi sp, sp, 8 # free space for args of function "temp4"
  sw a0, 8(sp)
  lw a0, 8(sp)
  call rukaml_print_int
  sw a0, (sp)
  li a0, 0
  addi sp, sp, 8*3 # deallocate local variables g, f, temp4
  addi    a0, x0, 0   # Use 0 return code
  addi    a7, x0, 93  # Service command code 93 terminates
  ecall               # Call linux to terminate the program
