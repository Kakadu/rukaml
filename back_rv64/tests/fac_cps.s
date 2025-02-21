.globl id
id:
  ld t5, (sp)
  addi a0, t5, 0
  ret # id

## let k0 k n m = k (n*m)
.globl k0
k0:
  addi sp, sp, -16  # RA + padding
  sd ra, 8(sp)
  ld t0, 24(sp) # n
  ld t1, 32(sp) # m
  mulw t2, t0, t1 # t2 = n*m

   #li t0, 102
  sd t2, (sp)
  call rukaml_print_int
  ld ra, 8(sp)

  ld a0, 16(sp) # k
  li a1, 1
  addi a2, t2, 0
  call rukaml_applyN  # result in a0
  ld ra, 8(sp)
  addi sp, sp, 16 # free space for  RA + padding
  ret # k0

## let rec faccps n k =
##   if n=1 then k 1
##   else faccps (n-1) (k0 k n)

.globl faccps
faccps:
  ld t0, (sp) # n
  li t1, 1
  beq t0, t1, lab_33
  beq zero, zero, lab_else_34
lab_33: # then
  addi sp, sp, -16 # RA and padding
  sd ra, 8(sp)
  ld a0, 24(sp) # access k
  li a1, 1    # argc = 1
  li a2, 1    # 1!
  call rukaml_applyN # answer is in a0
  ld ra, 8(sp)
  addi sp, sp, 16 # free space for ra and CLO for k
  # Should we remove args from stack? No
  ret

lab_else_34: # else is recursive call
  addi sp, sp, -16 #  RA + k0_clo                       # 2 locals
  sd ra, 8(sp)
  # wrapping k0 into closure
  lla a0, k0
  li a1, 3
  call rukaml_alloc_closure
  sd a0, (sp)
  ld ra, 8(sp)  # Is it needed?
  # call k0 k n
  # a0 is already a Clo(k0)
  li a1, 2
  ld a2, 24(sp)  # k
  ld a3, 16(sp) # n
  call rukaml_applyN # new cont is in a0
  ld ra, 8(sp)  # Is it needed?
  addi sp, sp, -16 #  faccps args                      # 4 locals
  sd a0, 8(sp)
  ld t0, 32(sp) # access n
  addi t0, t0, -1 # n-1
  sd t0, (sp)   # store n
  call faccps  # result in a0
  addi sp, sp,  16 # remove faccps args                # 2 locals
  ld ra, 8(sp)
  addi sp, sp, 16 # clear stack                        # 0 locals
  ret # fac_cps

.globl main
main:
  addi sp, sp, -32 # allocate for local variables RA, z, w, Clo(id)
  sd ra, 24(sp)
  # allocate Clo(ID)
  lla a0, id
  li a1, 1
  call rukaml_alloc_closure
  sd a0, (sp)
# Allocate args to call fun "fib" arguments
  addi sp, sp, -16  # fib args,                      # 48 on stack
  sd a0, 8(sp)  # id
  li t0, 4
  sd t0, (sp)   # n = 2
  call faccps   # result in a0
  addi sp, sp, 16 # deallocate fib args              # 32 on stack
  sd a0, (sp)
  call rukaml_print_int # result in a0
  ld ra, 24(sp)
  addi sp, sp, 32                                   # 0 on stack
  li a0, 0
  addi sp, sp, 24 # deallocate local variables z, w, temp17
  addi a0, x0, 0 # Use 0 return code
  addi a7, x0, 93 # Service command code 93 terminates
  ecall # Call linux to terminate the program
