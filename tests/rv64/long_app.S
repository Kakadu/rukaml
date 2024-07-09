.globl wrap
wrap:
  addi sp, sp, -8 # allocate for local variables temp1
  li t0, 1
  sd t0, (sp)
  ld t0, (sp)
  beq t0, zero, lab_else_31
  ld t5, 8(sp)
  addi a0, t5, 0
  beq zero, zero, lab_endif_32
lab_else_31: # temp1 is 0
  ld t5, 8(sp)
  addi a0, t5, 0
lab_endif_32:
  addi sp, sp, 8 # deallocate local variables temp1
  ret # wrap
.globl test3
test3:
  addi sp, sp, -24 # allocate for local variables c, b, a
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 32(sp)
  sd ra, (sp)
  call rukaml_print_int
  sd a0, 24(sp)
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 40(sp)
  sd ra, (sp)
  call rukaml_print_int
  sd a0, 16(sp)
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 48(sp)
  sd ra, (sp)
  call rukaml_print_int
  sd a0, 8(sp)
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  li a0, 0
  addi sp, sp, 24 # deallocate local variables c, b, a
  ret # test3
.globl test10
test10:
  addi sp, sp, -64 # allocate for local variables temp13, temp12, temp11, temp10, temp9, temp8, temp7, temp6
# a is stored in 0
# b is stored in -1
# last_pos = 8
  ld t3, 64(sp)
  ld t4, 72(sp)
  add  t5, t3, t4
  sd t5, 56(sp)
# temp6 is stored in 1
# c is stored in -2
# last_pos = 8
  ld t3, 56(sp)
  ld t4, 80(sp)
  add  t5, t3, t4
  sd t5, 48(sp)
# temp7 is stored in 2
# d is stored in -3
# last_pos = 8
  ld t3, 48(sp)
  ld t4, 88(sp)
  add  t5, t3, t4
  sd t5, 40(sp)
# temp8 is stored in 3
# e is stored in -4
# last_pos = 8
  ld t3, 40(sp)
  ld t4, 96(sp)
  add  t5, t3, t4
  sd t5, 32(sp)
# temp9 is stored in 4
# f is stored in -5
# last_pos = 8
  ld t3, 32(sp)
  ld t4, 104(sp)
  add  t5, t3, t4
  sd t5, 24(sp)
# temp10 is stored in 5
# g is stored in -6
# last_pos = 8
  ld t3, 24(sp)
  ld t4, 112(sp)
  add  t5, t3, t4
  sd t5, 16(sp)
# temp11 is stored in 6
# h is stored in -7
# last_pos = 8
  ld t3, 16(sp)
  ld t4, 120(sp)
  add  t5, t3, t4
  sd t5, 8(sp)
# temp12 is stored in 7
# i is stored in -8
# last_pos = 8
  ld t3, 8(sp)
  ld t4, 128(sp)
  add  t5, t3, t4
  sd t5, (sp)
# temp13 is stored in 8
# j is stored in -9
# last_pos = 8
  ld t3, (sp)
  ld t4, 136(sp)
  add  t5, t3, t4
  addi a0, t5, 0
  addi sp, sp, 64 # deallocate local variables temp13, temp12, temp11, temp10, temp9, temp8, temp7, temp6
  ret # test10
.globl main
main:
  addi sp, sp, -128 # allocate for local variables temp2, temp29, temp28, temp27, temp1, temp0, temp24, temp23, temp22, temp21, temp20, temp19, temp18, temp17, temp16, temp15
  addi sp, sp, -8 # alloc space for RA register
  sd ra, (sp)
# Allocate args to call fun "wrap" arguments
  addi sp, sp, -8
  lla a0, test10
  li a1, 10
  call rukaml_alloc_closure
  sd a0, (sp)
  call wrap
  addi sp, sp, 8 # deallocate 1 args
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  sd a0, 120(sp)
  addi sp, sp, -8 # first arg of a function temp15
  li t0, 1
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 136(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp15"
  sd a0, 112(sp)
  addi sp, sp, -8 # first arg of a function temp16
  li t0, 10
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 128(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp16"
  sd a0, 104(sp)
  addi sp, sp, -8 # first arg of a function temp17
  li t0, 100
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 120(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp17"
  sd a0, 96(sp)
  addi sp, sp, -8 # first arg of a function temp18
  li t0, 1000
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 112(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp18"
  sd a0, 88(sp)
  addi sp, sp, -8 # first arg of a function temp19
  li t0, 10000
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 104(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp19"
  sd a0, 80(sp)
  addi sp, sp, -8 # first arg of a function temp20
  li t0, 100000
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 96(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp20"
  sd a0, 72(sp)
  addi sp, sp, -8 # first arg of a function temp21
  li t0, 1000000
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 88(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp21"
  sd a0, 64(sp)
  addi sp, sp, -8 # first arg of a function temp22
  li t0, 10000000
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 80(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp22"
  sd a0, 56(sp)
  addi sp, sp, -8 # first arg of a function temp23
  li t0, 100000000
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 72(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp23"
  sd a0, 48(sp)
  addi sp, sp, -8 # first arg of a function temp24
  li t0, 1000000000
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 64(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp24"
  sd a0, 40(sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 48(sp)
  sd ra, (sp)
  call rukaml_print_int
  sd a0, 40(sp)
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, -8 # alloc space for RA register
  sd ra, (sp)
# Allocate args to call fun "wrap" arguments
  addi sp, sp, -8
  lla a0, test3
  li a1, 3
  call rukaml_alloc_closure
  sd a0, (sp)
  call wrap
  addi sp, sp, 8 # deallocate 1 args
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  sd a0, 24(sp)
  addi sp, sp, -8 # first arg of a function temp27
  li t0, 1
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 40(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp27"
  sd a0, 16(sp)
  addi sp, sp, -8 # first arg of a function temp28
  li t0, 10
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 32(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp28"
  sd a0, 8(sp)
  addi sp, sp, -8 # first arg of a function temp29
  li t0, 100
  sd t0, (sp)
  addi sp, sp, -8 # alloc space for RA register
  ld a0, 24(sp)
  li a1, 1
  ld a2, 8(sp)
  sd ra, (sp)
  call rukaml_applyN
  ld ra, (sp)
  addi sp, sp, 8 # free space of RA register
  addi sp, sp, 8 # free space for args of function "temp29"
  sd a0, (sp)
  li a0, 0
  addi sp, sp, 128 # deallocate local variables temp2, temp29, temp28, temp27, temp1, temp0, temp24, temp23, temp22, temp21, temp20, temp19, temp18, temp17, temp16, temp15
  addi a0, x0, 0 # Use 0 return code
  addi a7, x0, 93 # Service command code 93 terminates
  ecall # Call linux to terminate the program
