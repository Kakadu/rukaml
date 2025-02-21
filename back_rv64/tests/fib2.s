.globl id
id:
  ld t5, (sp)
  addi a0, t5, 0
  ret # id
.globl k2
k2:
  addi sp, sp, -8 # allocate for local variables w
  addi sp, sp, -16
  li a0, 102
  sd a0, (sp)
  sd ra, 8(sp)
  call rukaml_print_int
  ld ra, 8(sp)
  sd a0, 16(sp)
  addi sp, sp, 16
  addi sp, sp, -16 # RA and 1st arg of function k1
  sd ra, 8(sp)
  ld t5, 32(sp)
  sd t5, (sp) # access a var "t3"
  ld a0, 24(sp)
  li a1, 1
  ld a2, (sp)
  call rukaml_applyN
  ld ra, 8(sp)
  addi sp, sp, 16 # free space for ra and arg 1 of function "k1"
  addi sp, sp, 8 # deallocate local variables w
  ret # k2

.globl k3
k3:
  addi sp, sp, -16 # allocate for RA + k2_clo/z          # 2 on stack
  sd ra, 8(sp)

  lla a0, k2
  li a1, 2
  call rukaml_alloc_closure
  sd a0, (sp)
  # Allocate args to call fun "k2" arguments
  addi sp, sp, -16 # padding + k                         # 4 on stack
  ld t0, 48(sp) # arg "k"
  sd t0, (sp)
  ld a0, 16(sp) # access k2_clo
  li a1, 1
  ld a2, (sp) # arg 0 a.k.a k
  call rukaml_applyN # z is in a0
  addi sp, sp, 16 # Remove padding + k                   # 2 on stack
  sd a0, (sp)  # z is saved

  addi sp, sp, -16 # padding + print_arg                 # 4 on stack
  li a0, 52
  sd a0, (sp)
  call rukaml_print_int
  # We ignore value of w
  addi sp, sp, 16                                        # 2 on stack
  # let's call fib
  addi sp, sp, -16                                       # 4 on stack
  ld t0, 16(sp) # access z
  sd t0, 8(sp)

  ld t0, 32(sp) # access n
  addi t0, t0, -1
  sd t0, (sp)

  call fib
  ld ra, 32(sp)
  addi sp, sp, 48                                        # 0 on stack
  ret # k3

.globl fib
fib:
  ld t0, (sp) # n
  li t1, 2
  blt t0, t1, lab_33
  beq zero, zero, lab_else_34
lab_33: # then
  addi sp, sp, -16 # RA and CLO for k
  sd ra, 8(sp)
  ld a0, 24(sp) # access k
  li a1, 1
  ld a2, 16(sp) # access n
  addi a2, a2, 100   # ONLY FOR DEBUG
  call rukaml_applyN # answer is in a0
  ld ra, 8(sp)
  addi sp, sp, 16 # free space for ra and CLO for k
  # Should we remove args from stack? No
  ret
lab_else_34: # temp8 is 0
  addi sp, sp, -32 #  RA + fib_clo + k3_clo + padding    # 4 locals
  sd ra, 24(sp)
  # wrapping k3 into closure
  lla a0, k3
  li a1, 4
  call rukaml_alloc_closure
  sd a0, 8(sp)
  # wrapping fib into closure
  lla a0, fib
  li a1, 2
  call rukaml_alloc_closure
  sd a0, 16(sp)
  # calculate z
  ld a0, 8(sp) # k3_clo
  li a1, 3
  ld a2, 16(sp)  # fib_clo
  ld a3, 32(sp) # access a var "n" ???
  ld a4, 40(sp) # access a var "k" ???
  call rukaml_applyN # z in RA
  #ld ra, 16(sp)  # restore RA
  # prepare to call fib
  # reorder to save moves
  mv a3, a0  # second argument is z
  ld a0, 16(sp) # we call closure for fib
  li a1, 2
  ld a2, 40(sp) # access "n"
  addi a2, a2, -1 # decrement n
  # a3 is already set
  call rukaml_applyN # result in a0
  ld ra, 24(sp) # restore RA
  addi sp, sp, 32 # clear stack
  ret # fib

.globl main
main:
  addi sp, sp, -24 # allocate for local variables z, w, temp17
  addi sp, sp, -16 #  RA + closure
  sd ra, 8(sp)
  lla a0, fib
  li a1, 2
  call rukaml_alloc_closure
  sd a0, (sp)
# Allocate args to call fun "fib" arguments
  addi sp, sp, -8
  li t0, 2  # fib arg
  sd t0, (sp) # constant
  ld a0, 8(sp)
  li a1, 1
  ld a2, (sp) # arg 0
  call rukaml_applyN
  sd a0, 40(sp)
  addi sp, sp, 8 # deallocate 1 args
  ld ra, 8(sp)
  addi sp, sp, 16 # deallocate RA + closure
  addi sp, sp, -16 # RA and 1st arg of function temp17
  sd ra, 8(sp)
  lla a0, id
  li a1, 1
  call rukaml_alloc_closure
  sd a0, (sp)
  ld a0, 32(sp)
  li a1, 1
  ld a2, (sp)
  call rukaml_applyN
  ld ra, 8(sp)
  addi sp, sp, 16 # free space for ra and arg 1 of function "temp17"
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
  addi sp, sp, 24 # deallocate local variables z, w, temp17
  addi a0, x0, 0 # Use 0 return code
  addi a7, x0, 93 # Service command code 93 terminates
  ecall # Call linux to terminate the program
