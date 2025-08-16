  $ ../../back/rv64/RV64_compiler.exe -o program.s --no-start -danf add.ml
  After ANF transformation.
  let add a b =
    (a + b)
  let main =
    let x = add 1 10 in
      let y = print x  in
        0
  $ cat program.s | grep -v 'section .note.GNU-stack'
  
  .globl add
  add:
    addi sp, sp, -16 # allocate for Pad, RA, and 0 locals 
    sd ra, (sp)
  # a is stored in 0
  # b is stored in -1
  # last_pos = 2
    ld t3, 16(sp)
    ld t4, 24(sp)
    add  t5, t3, t4
    addi a0, t5, 0
    ld ra, (sp)
    addi sp, sp, 16 # DEallocate for Pad, RA and 0 locals variables 
    ret # add
  
  .globl main
  main:
  # this is main
    addi sp, sp, -32 # allocate for Pad, RA, and 2 locals y, x
    sd ra, 16(sp)
  # Allocate args to call fun "add" with args
    addi sp, sp, -16 # last_pos = 6
    li t0, 1
    sd t0, (sp) # constant
    li t0, 10
    sd t0, 8(sp) # constant
    call add
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
    addi sp, sp, 32 # DEallocate for Pad, RA and 2 locals variables y, x
  #  fin
    addi a0, x0, 0 # Use 0 return code
    addi a7, x0, 93 # Service command code 93 terminates
    ecall # Call linux to terminate the program

$ riscv64-linux-gnu-gcc-13 -c -g program.s -o program.o
$ riscv64-linux-gnu-gcc-13 -g -o program.exe ../../back/rv64/rukaml_stdlib.o program.o && ./program.exe
$ riscv64-linux-gnu-gcc-13 -g program.o ../../back/rv64/rukaml_stdlib.o -o fib_acc.exe 2>&1 | head -n5
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64  ./add.exe
  rukaml_print_int 11
