
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'
  $ ls
  compiler.exe
  program0.c
$ chmod +w program.c
$ echo 'int main(){return;}' > program.c
$ cp program0.c program.c
$ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target parsetree




  $ qemu-riscv64 $FLAGS ./compiler.exe -dc program0.c -o file.out --target rv64
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
  
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 13170(words)
  Currently allocated: 13170(words)
  Current bank: 0
  .global fact_rec
  .text
  fact_rec:
    addi sp, sp, -8
    sd fp, 4(sp)
    sd ra, 0(sp)
    mv fp, sp
  addi sp, sp, 0
    ld a0, 8(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 1
    ld t0, 0(sp)
    addi sp, sp, 4 # 1 
    slt a0, t0, a0
    beqz a0, End_if_1
    li a0, 1
    j fact_rec_epilogue_0
  End_if_1: 
    ld a0, 8(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -4
    ld a0, 8(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 1
    ld t0, 0(sp)
    addi sp, sp, 4 # 1 
    sd a0, 0(sp)
    call fact_rec
    addi sp, sp, 4
    ld t0, 0(sp)
    addi sp, sp, 4 # 1 
    j fact_rec_epilogue_0
  addi sp, sp, 0
  #store has 41 instrs
  fact_rec:# 0
    addi sp, sp, -8# 1
    sw fp, 4(sp)# 2
    sw ra, 0(sp)# 3
    addi fp, sp, 0# 4
    addi sp, sp, 0# 5
    lw a0, 8(fp)# 6
    addi sp, sp, -4# 7
    sw a0, 0(sp)# 8
    li a0, 1# 9
    lw t0, 0(sp)# 10
    addi sp, sp, 4# 11
    slt a0, t0, a0# 12
    beqz a0, End_if_1# 13
    li a0, 1# 14
    j fact_rec_epilogue_0# 15
  End_if_1:# 16
    lw a0, 8(fp)# 17
    addi sp, sp, -4# 18
    sw a0, 0(sp)# 19
    addi sp, sp, -4# 20
    lw a0, 8(fp)# 21
    addi sp, sp, -4# 22
    sw a0, 0(sp)# 23
    li a0, 1# 24
    lw t0, 0(sp)# 25
    addi sp, sp, 4# 26
    sub a0, t0, a0# 27
    sw a0, 0(sp)# 28
    call fact_rec# 29
    addi sp, sp, 4# 30
    lw t0, 0(sp)# 31
    addi sp, sp, 4# 32
    mul a0, t0, a0# 33
    j fact_rec_epilogue_0# 34
  fact_rec_epilogue_0:# 35
    addi sp, sp, 0# 36
    lw ra, 0(sp)# 37
    lw fp, 4(sp)# 38
    addi sp, sp, 8# 39
    ret# 40
  count = 41
  jump offset = 18
  jump offset = -1
  fact_rec:
  # 0xff81 0113
            addi sp, sp, -8
  # 0x0081 2223
            sw fp, 4(sp)
  # 0x0011 2023
            sw ra, 0(sp)
  # 0x0001 0413
            addi fp, sp, 0
  # 0x0001 0113
            addi sp, sp, 0
  # 0x0084 2503
            lw a0, 8(fp)
  # 0xffc1 0113
            addi sp, sp, -4
  # 0x00a1 2023
            sw a0, 0(sp)
  # 0x0010 0513
            li a0, 1
  # 0x0001 2283
            lw t0, 0(sp)
  # 0x0041 0113
            addi sp, sp, 4
  # 0x00a2 a533
            slt a0, t0, a0
  # 0xffff ffff
            beqz a0, End_if_1
  # 0x0010 0513
            li a0, 1
  # 0xffff ffff
            j fact_rec_epilogue_0
  End_if_1:
  # 0x0084 2503
            lw a0, 8(fp)
  # 0xffc1 0113
            addi sp, sp, -4
  # 0x00a1 2023
            sw a0, 0(sp)
  # 0xffc1 0113
            addi sp, sp, -4
  # 0x0084 2503
            lw a0, 8(fp)
  # 0xffc1 0113
            addi sp, sp, -4
  # 0x00a1 2023
            sw a0, 0(sp)
  # 0x0010 0513
            li a0, 1
  # 0x0001 2283
            lw t0, 0(sp)
  # 0x0041 0113
            addi sp, sp, 4
  # 0x40a2 8533
            sub a0, t0, a0
  # 0x00a1 2023
            sw a0, 0(sp)
  # 0xffff ffff
            call fact_rec
  # 0x0041 0113
            addi sp, sp, 4
  # 0x0001 2283
            lw t0, 0(sp)
  # 0x0041 0113
            addi sp, sp, 4
  # 0x02a2 8533
            mul a0, t0, a0
  # 0x0040 006f
            j fact_rec_epilogue_0
  fact_rec_epilogue_0:
  # 0x0001 0113
            addi sp, sp, 0
  # 0x0001 2083
            lw ra, 0(sp)
  # 0x0041 2403
            lw fp, 4(sp)
  # 0x0081 0113
            addi sp, sp, 8
  # 0x0000 8067
            ret
  GC statistics
  Total allocations: 23963(words)
  Currently allocated: 23963(words)
  Current bank: 0
