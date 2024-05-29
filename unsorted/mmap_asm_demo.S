# https://github.com/riscv/riscv-v-spec/blob/master/v-spec.adoc
# https://jborza.com/post/2021-05-11-riscv-linux-syscalls
.text
.balign 4

.global memset
memset: # a0 is addr, a1 is byte, a2 is length
memset_loop:
  beq a2, zero, memset_fin
  sb a1, (a0)
  addi a0, a0, 1
  addi a2, a2, -1
  j memset_loop
memset_fin:
  ret

.global myitoa
myitoa:
  # t2 is an arg, a0 is current pos, a1 is output len, a2 -- dest
  li t1, 10     # t1 is basis
  mv t2, a0     # argument
  mv a1, zero   # output length
  mv a0, a2
  addi a0, a0, 32  # 32 is preallocated space
myitoa_loop:
  #
  remu t4, t2, t1
  addi t4, t4, 0x30   # t4 stores one more char
  addi a1, a1, 1
  addi a0, a0, -1
  sb t4, (a0)
  divu t2, t2, t1
  bne t2, zero, myitoa_loop
  ret

.global _start
_start:
    .option push
    .option norelax
    la gp, __global_pointer$
    .option pop
    li      a7, 64  # write on RISCV linux
    li      a0, 1
    la      a1, helloworld
    li      a2, 13
    ecall

    li a7, 222  # mmap
    li a0, 0    # destination hint
    li a1, 64
    li a2, 3 # PROT_READ | PROT_WRITE
    li a3, 33 #  MAP_SHARED | MAP_ANONYMOUS
    li a4, -1
    li a5, 0
    ecall
    addi sp, sp, -8
    sd a0, (sp)

    li a5, -1
    beq a0,a5, main_map_failed
    #set by zeros
    ld a0, (sp)
    li a1, 0
    li a2, 12
    call memset

    ld a0, (sp)
    addi a0, a0, 11
    li t0, 0xA # '\n'
    sb t0, (a0)
    # set by '0'
    ld a0, (sp)
    li a1, 0x30
    li a2, 10
    call memset

    li a7, 64  # write on RISCV linux
    li a0, 1
    ld a1, (sp)
    li a2, 12
    ecall

    li t5, 10  # counter
    ld t4, (sp) # curpos
    li t1, 0x30 # char zero
fill_loop:
    beq t5, zero, fill_loop_fin
    addi t5, t5, -1
    add t2, t1, t5
    sb t2, (t4)
    addi t4, t4, 1
    j fill_loop
fill_loop_fin:
    # print after filling
    li a7, 64  # write on RISCV linux
    li a0, 1
    ld a1, (sp)
    li a2, 12
    ecall

    li      a7, 93  # Service command code 93 terminates
    li      a0, 1   # Use 0 return code
    ecall           # Call linux to terminate the program

main_map_failed:
    li      a7, 93  # Service command code 93 terminates
    li      a0, 13  # Use 0 return code
    ecall           # Call linux to terminate the program

.data
helloworld:      .ascii "Hello World!\n\0"
