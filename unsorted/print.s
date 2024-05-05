# https://github.com/riscv/riscv-v-spec/blob/master/v-spec.adoc
.section .data
helloworld: .string "Hello_World!\n\0"
varname:    .string "varname\n"
str2:       .string "01234567012345670123456701234567\n" # 32+1 bytes
.equ BUFSIZE,32
.text

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

.global strlen
strlen: # a0 is a string, a0 is a result
  li t0, 0
strlen_loop:
  ld t1, (a0)
  bne zero, t1, strlen_fin
  addi a0, a0, 1
  addi t0, t0, 1
  j strlen_loop
strlen_fin:
  mv a0, t0
  ret

.global memcpy
memcpy: # a0 dest, a1 src, a2 size. No intersection
memcpy_loop:
  beq a2, zero, memcpy_fin
  lb t0, (a1)
  sb t0, (a0)
  addi a0, a0, 1
  addi a1, a1, 1
  addi a2, a2, -1
  j memcpy_loop
memcpy_fin:
  ret

.global myitoa
myitoa:
  # t2 is an arg, a0 is current pos, a1 is output len
  #li t5, 0x30   #  char for an '0'
  li t1, 10     # t1 is basis
  mv t2, a0     # argument
  mv a1, zero   # output length
  la a0, str2
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
    li      a0, 1   # stdout
    la      a1, helloworld
    li      a2, 14
    ecall

    li      a7, 64  # write on RISCV linux
    li      a0, 1
    la      a1, str2
    #li      a2, 10 #
    li      a2, BUFSIZE+1
    ecall

    la a0, str2
    li a1, 0x20 # space
    li a2, BUFSIZE+1
    call memset

    li a0, 65536
    call myitoa

    la a0, str2
    la a1, varname
    li a2, 7
    call memcpy

    li      a7, 64  # write on RISCV linux
    li      a0, 1
    la      a1, str2
    li      a2, BUFSIZE+1
    ecall

    li      a7, 93  # Service command code 93 terminates
    li      a0, 0   # Use 0 return code
    ecall           # Call linux to terminate the program

