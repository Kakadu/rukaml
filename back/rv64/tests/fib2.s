.globl id
id:
  ld t5, (sp)
  addi a0, t5, 0
  ret # id

## let k2 k m n =
##  let w = print 102 in
##  k (m+n)
.globl k2
k2:
  addi sp, sp, -16   # Allocate for RA + print_arg    # 2 on stack
  sd ra, 8(sp)
  li a0, 102
  sd a0, (sp)
  call rukaml_print_int
  ld ra, 8(sp)

  ld t0, 24(sp) # m
  ld t1, 32(sp) # n
  ld a0, 16(sp) # k
  li a1, 1
  add a2, t0, t1
  call rukaml_applyN # result in a0
  ld ra, 8(sp)
  addi sp, sp, 16 # free space for ra and arg 1 of function "k1"
  ret # k2

## let k3 fib n k t2 =
##   let z = k2 k t2 in
##   let w = print 52 in
##   fib (n - 2) z
.globl k3
k3:
  addi sp, sp, -16 # allocate for RA + k2_clo/z          # 2 on stack
  sd ra, 8(sp)

  lla a0, k2
  li a1, 3
  call rukaml_alloc_closure
  ld ra, 8(sp)  # Not needed?!
  sd a0, (sp)

  # Allocate args to prepare fun "k2" arguments
  ld a0, (sp) # access k2_clo
  li a1, 2
  ld a2, 32(sp)  # arg 0 a.k.a 'k'
  ld a3, 40(sp)  # arg 1 a.k.a 't2'
  call rukaml_applyN # z is in a0
  sd a0, (sp)  # z is saved
  ld ra, 8(sp)  # Not needed?!

  addi sp, sp, -16 # padding + print_arg                 # 4 on stack
  li a0, 52
  sd a0, (sp)
  call rukaml_print_int
  # We ignore value of w
  addi sp, sp, 16                                        # 2 on stack
  
  ld ra, 8(sp)  # Not needed?!
  # let's call fib
  addi sp, sp, -16                                       # 4 on stack
  ld t0, 16(sp) # access z
  sd t0, 8(sp)

  ld t0, 40(sp) # access n
  addi t0, t0, -2
  sd t0, (sp)
  call fib
  ld ra, 24(sp)
  addi sp, sp, 32                                        # 0 on stack
  ret # k3


## let rec fibcps n k =
##   if n<2 then k 1
##   else let z = k3 fib n k in
##        fib (n - 1) z
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
  # addi a2, a2, 100   # ONLY FOR DEBUG
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
  ld a2, 32(sp) # access "n"
  addi a2, a2, -1 # decrement n
  # a3 is already set
  call rukaml_applyN # result in a0
  ld ra, 24(sp) # restore RA
  addi sp, sp, 32 # clear stack
  ret # fib

## let main =
##  let w = fib 2 id in
##  let z = print w in
##  0
.globl test_main
test_main:
  addi sp, sp, -16 # allocate for RA + Clo(id)/ribrez     # 2 on stack
  sd ra, 8(sp)
  # allocate Clo(ID)
  lla a0, id
  li a1, 1
  call rukaml_alloc_closure
  sd a0, (sp)
  ##ld ra, 8(sp)
  # Allocate args to call fun "fib" arguments
  addi sp, sp, -16  # fib args,                      # 4 on stack
  li t0, 6
  sd t0, (sp)   # input n 
  sd a0, 8(sp)  # id
  call fib   # result in a0  
  addi sp, sp, 16 # deallocate fib args              # 2 on stack
  ##ld ra, 8(sp)
  sd a0, (sp)
  call rukaml_print_int # result in a0
  ld ra, 8(sp)
  ld a0, (sp)
  addi sp, sp, 16                                   # 0 on stack
  ret

.globl test_id
test_id:
  addi sp, sp, -16
  li t0, 42
  sd t0, (sp)
  sd ra, 8(sp)
  call id
  sd a0, (sp)
  call rukaml_print_int
  ld ra, 8(sp)
  addi sp, sp, 16
  li a0, 0
  ret

.globl test_k2
test_k2:
  addi sp, sp, -16 # allocate for RA + Clo(id)/     # 2 on stack
  sd ra, 8(sp)
  # allocate Clo(ID)
  lla a0, id
  li a1, 1
  call rukaml_alloc_closure
  sd a0, (sp)
  addi sp, sp, -32 # allocate for padding+n+m+k     # 6 on stack
  li t0, 10
  sd t0, 16(sp) # n
  li t1, 1
  sd t1, 8(sp)
  sd a0, (sp) # clo(id)
  call k2
  sd a0, (sp)
  call rukaml_print_int
  addi sp, sp, 32
  ld ra, 8(sp)
  addi sp, sp, 16
  ret

.globl test_k3
test_k3:
  addi sp, sp, -32 # RA, fib_clo, id_clo, padding
  sd ra, 24(sp)
  # allocate Clo(ID)
  lla a0, id
  li a1, 1
  call rukaml_alloc_closure
  sd a0, 8(sp)
  # allocate Clo(fib)
  lla a0, fib
  li a1, 1
  call rukaml_alloc_closure
  sd a0, 16(sp)
  # alloc k3 args
  addi sp, sp, -32
  ld t0, 48(sp) # fib
  sd t0, (sp)   # fib
  li t0, 3      # n 
  sd t0, 8(sp)
  ld t0, 40(sp) # k
  sd t0, 16(sp) # k
  li t0, 2       # t2
  sd t0, 24(sp) # t2
  call k3

  sd a0, (sp)
  call rukaml_print_int
  addi sp, sp, 32

  ld ra, 24(sp)
  addi sp, sp, 32 
  ret

.globl main
main:
  call test_main
  #call test_id
  #call test_k2
  #call test_k3
  #li a0, 0
  #addi sp, sp, 24 # deallocate local variables z, w, temp17
  addi a0, x0, 0 # Use 0 return code
  addi a7, x0, 93 # Service command code 93 terminates
  ecall # Call linux to terminate the program
