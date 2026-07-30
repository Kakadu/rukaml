
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
  Total allocations: 13176(words)
  Currently allocated: 13176(words)
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
    beq a0, zero, End_if_1# 13
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
   fact_rec_epilogue_0 -> 33
   End_if_1 -> 15
   fact_rec -> 0
  jump offset = 18
  jump offset = -1
  /* fact_rec: */
  m[0] = (0xff81u << 16) + 0x0113u;  /* 1:   addi sp, sp, -8 */
  m[1] = (0x0081u << 16) + 0x2223u;  /* 2:   sw fp, 4(sp) */
  m[2] = (0x0011u << 16) + 0x2023u;  /* 3:   sw ra, 0(sp) */
  m[3] = (0x0001u << 16) + 0x0413u;  /* 4:   addi fp, sp, 0 */
  m[4] = (0x0001u << 16) + 0x0113u;  /* 5:   addi sp, sp, 0 */
  m[5] = (0x0084u << 16) + 0x2503u;  /* 6:   lw a0, 8(fp) */
  m[6] = (0xffc1u << 16) + 0x0113u;  /* 7:   addi sp, sp, -4 */
  m[7] = (0x00a1u << 16) + 0x2023u;  /* 8:   sw a0, 0(sp) */
  m[8] = (0x0010u << 16) + 0x0513u;  /* 9:   li a0, 1 */
  m[9] = (0x0001u << 16) + 0x2283u;  /* 10:   lw t0, 0(sp) */
  m[10] = (0x0041u << 16) + 0x0113u;  /* 11:   addi sp, sp, 4 */
  m[11] = (0x00a2u << 16) + 0xa533u;  /* 12:   slt a0, t0, a0 */
  m[12] = (0x0005u << 16) + 0x0663u;  /* 13:   beq a0, zero, End_if_1 */
  m[13] = (0x0010u << 16) + 0x0513u;  /* 14:   li a0, 1 */
  m[14] = (0x04c0u << 16) + 0x006fu;  /* 15:   j fact_rec_epilogue_0 */
  /* End_if_1: */
  m[15] = (0x0084u << 16) + 0x2503u;  /* 16:   lw a0, 8(fp) */
  m[16] = (0xffc1u << 16) + 0x0113u;  /* 17:   addi sp, sp, -4 */
  m[17] = (0x00a1u << 16) + 0x2023u;  /* 18:   sw a0, 0(sp) */
  m[18] = (0xffc1u << 16) + 0x0113u;  /* 19:   addi sp, sp, -4 */
  m[19] = (0x0084u << 16) + 0x2503u;  /* 20:   lw a0, 8(fp) */
  m[20] = (0xffc1u << 16) + 0x0113u;  /* 21:   addi sp, sp, -4 */
  m[21] = (0x00a1u << 16) + 0x2023u;  /* 22:   sw a0, 0(sp) */
  m[22] = (0x0010u << 16) + 0x0513u;  /* 23:   li a0, 1 */
  m[23] = (0x0001u << 16) + 0x2283u;  /* 24:   lw t0, 0(sp) */
  m[24] = (0x0041u << 16) + 0x0113u;  /* 25:   addi sp, sp, 4 */
  m[25] = (0x40a2u << 16) + 0x8533u;  /* 26:   sub a0, t0, a0 */
  m[26] = (0x00a1u << 16) + 0x2023u;  /* 27:   sw a0, 0(sp) */
  m[27] = (0xf95fu << 16) + 0xf0efu;  /* 28:   call fact_rec */
  m[28] = (0x0041u << 16) + 0x0113u;  /* 29:   addi sp, sp, 4 */
  m[29] = (0x0001u << 16) + 0x2283u;  /* 30:   lw t0, 0(sp) */
  m[30] = (0x0041u << 16) + 0x0113u;  /* 31:   addi sp, sp, 4 */
  m[31] = (0x02a2u << 16) + 0x8533u;  /* 32:   mul a0, t0, a0 */
  m[32] = (0x0040u << 16) + 0x006fu;  /* 33:   j fact_rec_epilogue_0 */
  /* fact_rec_epilogue_0: */
  m[33] = (0x0001u << 16) + 0x0113u;  /* 34:   addi sp, sp, 0 */
  m[34] = (0x0001u << 16) + 0x2083u;  /* 35:   lw ra, 0(sp) */
  m[35] = (0x0041u << 16) + 0x2403u;  /* 36:   lw fp, 4(sp) */
  m[36] = (0x0081u << 16) + 0x0113u;  /* 37:   addi sp, sp, 8 */
  m[37] = (0x0000u << 16) + 0x8067u;  /* 38:   ret */
  GC statistics
  Total allocations: 25024(words)
  Currently allocated: 25024(words)
  Current bank: 0
