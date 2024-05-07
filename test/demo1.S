.section .data
helloworld: .string "Hello_World!\n\0"
.equ BUFSIZE,32
varname_acc: .string  "acc\n"
.equ VARLEN_acc, 3+1
varname_n: .string  "n\n"
.equ VARLEN_n, 1+1
.text
.extern memset
.extern trace_variable
.global _start
_start:
  .option push
  .option norelax
  la gp, __global_pointer$
  .option pop
  addi sp, sp, -16
  li t0, 1
  sd t0, (sp)
  li t0, 6
  sd t0, 8(sp)
lab1:
# (EBinop (">", (EVar "n"), (EConst 1)))
# Not implemented 29
  beq t0, zero, lab2
  ld t3, (sp)
  ld t4, 8(sp)
  mulw t0, t3, t4
  sd t0, (sp)
  ld t3, 8(sp)
  li t4, 1
  sub t0, t3, t4
  sd t0, 8(sp)
  j lab1
lab2:
# trace variable "acc"
  la a0, varname_acc
  li a1, 3
  ld a2, (sp)
  call trace_variable
# trace variable "n"
  la a0, varname_n
  li a1, 1
  ld a2, 8(sp)
  call trace_variable
  li a0, 22
  li a7, 93
  ecall

