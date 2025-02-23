  $ ../../back_rv64/RV64_compiler.exe -o program.s --no-start -danf -stop-after anf pass_print2.ml
  After ANF transformation.
  let addk a b k =
    let temp1 = (a + b) in
      k temp1 
  let foo f x =
    f x 
  let main =
    let tmp = foo print  in
      addk 1 10 tmp
  $ cat pass_print2.s | grep -v 'section .note.GNU-stack'
  
  .globl addk
  addk:
    addi sp, sp, -16 # allocate for RA, and 1 locals temp1
    sd ra, 8(sp)
  # a is stored in 0
  # b is stored in -1
  # last_pos = 2
    ld t3, 16(sp)
    ld t4, 24(sp)
    add  t5, t3, t4
    sd t5, (sp)
    addi sp, sp, -16 # pad and 1st arg of function k
    ld t5, 16(sp)
    sd t5, (sp) # access a var "temp1"
    ld a0, 48(sp)
    li a1, 1
    ld a2, (sp)
    call rukaml_applyN
    addi sp, sp, 16 # DEalloc for pad and arg 1 of function "k"
    ld ra, 8(sp)
    addi sp, sp, 16 # DEallocate for RA, and 1 locals temp1
    ret # addk
  
  .globl foo
  foo:
    addi sp, sp, -16 # allocate for Pad, RA, and 0 locals 
    sd ra, (sp)
    addi sp, sp, -16 # pad and 1st arg of function f
    ld t5, 40(sp)
    sd t5, (sp) # access a var "x"
    ld a0, 32(sp)
    li a1, 1
    ld a2, (sp)
    call rukaml_applyN
    addi sp, sp, 16 # DEalloc for pad and arg 1 of function "f"
    ld ra, (sp)
    addi sp, sp, 16 # DEallocate for Pad, RA and 0 locals variables 
    ret # foo
  
  .globl main
  main:
  # this is main
    addi sp, sp, -16 # allocate for RA, and 1 locals tmp
    sd ra, 8(sp)
    addi sp, sp, -16 #  RA + closure
    sd ra, 8(sp)
    lla a0, foo
    li a1, 2
    call rukaml_alloc_closure
    sd a0, (sp)
  # Allocate args to call fun "foo" with args
    addi sp, sp, -16 # last_pos = 6
    lla a0, rukaml_print_int
    li a1, 1
    call rukaml_alloc_closure
    sd a0, (sp)
    ld a0, 16(sp)
    li a1, 1
    ld a2, (sp) # arg 0
    call rukaml_applyN
    sd a0, 32(sp)
    addi sp, sp, 16 # deallocate 1 args
    ld ra, 8(sp)
    addi sp, sp, 16 # deallocate RA + closure
  # Allocate args to call fun "addk" with args
    addi sp, sp, -32 # last_pos = 6
    li t0, 1
    sd t0, (sp) # constant
    li t0, 10
    sd t0, 8(sp) # constant
    ld t0, 32(sp) # arg "tmp"
    sd t0, 16(sp)
    call addk
    addi sp, sp, 32 # deallocate 3 args
    mv a0, a0
    ld ra, 8(sp)
    addi sp, sp, 16 # DEallocate for RA, and 1 locals tmp
  #  fin
    addi a0, x0, 0 # Use 0 return code
    addi a7, x0, 93 # Service command code 93 terminates
    ecall # Call linux to terminate the program


  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./pass_print2.exe
  rukaml_print_int 11
