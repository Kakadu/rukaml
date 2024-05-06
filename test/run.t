$ cat > asdf.pas <<-EOF
> x=6;
> acc=1;
> while x>0 do acc=acc*x; x=x-1; done
> EOF
  $ cat > asdf.pas <<-EOF
  > x=6;
  > EOF
  $ cat asdf.pas
  x=6;
  $ ../lib/driver.exe asdf.pas
  $ cat out.s
  .section .data
  helloworld: .string "Hello_World!\n\0"
  .equ BUFSIZE,32
  varname_x: .string  "x"
  .equ VARLEN_x, 1
  .text
  .extern memset
  .extern trace_variable
  .global _start
  _start:
    .option push
    .option norelax
    la gp, __global_pointer$
    .option pop
    addi sp, sp, -8
    li t0, 6
    sd t0, (sp)
  # trace variable "x"
    la a0, varname_x
    ld a1, (sp)
    call trace_variable
    li a7, 64
    li a0, 1
    la a1, varname_x
    li a2, 1
    ecall
    li a0, 22
    li a7, 93
    ecall
  
  $ riscv64-linux-gnu-as -march=rv64gc_zbb out.s -o hello.o
  $ riscv64-linux-gnu-ld hello.o rv64_runtime.o -o program.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./program.exe
  x
  [22]
