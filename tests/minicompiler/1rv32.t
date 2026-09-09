

  $ chmod +w program.c

  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program.c -o file.out --target parsetree
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14464(words)
  Currently allocated: 14464(words)
  Current bank: 0
  int fact_rec(int n) {
    if ((n < 1)) {
    return 1;
  }
  
    return (n * fact_rec((n - 1)));
  }
  GC statistics
  Total allocations: 15172(words)
  Currently allocated: 15172(words)
  Current bank: 0









  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program.c -o file.out --target rv64
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14463(words)
  Currently allocated: 14463(words)
  Current bank: 0
  .global fact_rec
  .text
  fact_rec:
    addi sp, sp, -8
    sd fp, 4(sp)
    sd ra, 0(sp)
    mv fp, sp
    slt a0, t0, a0
  #store has 38 instrs
  fact_rec:# 0
    addi sp, sp, -8# 1
    sw fp, 4(sp)# 2
    sw ra, 0(sp)# 3
    lw a0, 8(fp)# 4
    addi sp, sp, -4# 5
    sw a0, 0(sp)# 6
    li a0, 1# 7
    lw t0, 0(sp)# 8
    addi sp, sp, 4# 9
    slt a0, t0, a0# 10
    beq a0, zero, End_if_1# 11
    li a0, 1# 12
    j fact_rec_epilogue_0# 13
  End_if_1:# 14
    lw a0, 8(fp)# 15
    addi sp, sp, -4# 16
    sw a0, 0(sp)# 17
    addi sp, sp, -4# 18
    lw a0, 8(fp)# 19
    addi sp, sp, -4# 20
    sw a0, 0(sp)# 21
    li a0, 1# 22
    lw t0, 0(sp)# 23
    addi sp, sp, 4# 24
    sub a0, t0, a0# 25
    sw a0, 0(sp)# 26
    call fact_rec# 27
    addi sp, sp, 4# 28
    lw t0, 0(sp)# 29
    addi sp, sp, 4# 30
    mul a0, t0, a0# 31
    j fact_rec_epilogue_0# 32
  fact_rec_epilogue_0:# 33
    lw ra, 0(sp)# 34
    lw fp, 4(sp)# 35
    addi sp, sp, 8# 36
    ret# 37
   fact_rec_epilogue_0 -> 31
   End_if_1 -> 13
   fact_rec -> 0
  /* fact_rec: */
  m[0] = (0xff81u << 16) + 0x113u;  /* 1:   addi sp, sp, -8 */
  m[1] = (0x81u << 16) + 0x2223u;  /* 2:   sw fp, 4(sp) */
  m[2] = (0x11u << 16) + 0x2023u;  /* 3:   sw ra, 0(sp) */
  m[3] = (0x84u << 16) + 0x2503u;  /* 4:   lw a0, 8(fp) */
  m[4] = (0xffc1u << 16) + 0x113u;  /* 5:   addi sp, sp, -4 */
  m[5] = (0xa1u << 16) + 0x2023u;  /* 6:   sw a0, 0(sp) */
  m[6] = (0x10u << 16) + 0x513u;  /* 7:   li a0, 1 */
  m[7] = (0x1u << 16) + 0x2283u;  /* 8:   lw t0, 0(sp) */
  m[8] = (0x41u << 16) + 0x113u;  /* 9:   addi sp, sp, 4 */
  m[9] = (0xa2u << 16) + 0xa533u;  /* 10:   slt a0, t0, a0 */
  m[10] = (0x5u << 16) + 0x663u;  /* 11:   beq a0, zero, End_if_1 */
  m[11] = (0x10u << 16) + 0x513u;  /* 12:   li a0, 1 */
  m[12] = (0x4c0u << 16) + 0x6fu;  /* 13:   j fact_rec_epilogue_0 */
  /* End_if_1: */
  m[13] = (0x84u << 16) + 0x2503u;  /* 14:   lw a0, 8(fp) */
  m[14] = (0xffc1u << 16) + 0x113u;  /* 15:   addi sp, sp, -4 */
  m[15] = (0xa1u << 16) + 0x2023u;  /* 16:   sw a0, 0(sp) */
  m[16] = (0xffc1u << 16) + 0x113u;  /* 17:   addi sp, sp, -4 */
  m[17] = (0x84u << 16) + 0x2503u;  /* 18:   lw a0, 8(fp) */
  m[18] = (0xffc1u << 16) + 0x113u;  /* 19:   addi sp, sp, -4 */
  m[19] = (0xa1u << 16) + 0x2023u;  /* 20:   sw a0, 0(sp) */
  m[20] = (0x10u << 16) + 0x513u;  /* 21:   li a0, 1 */
  m[21] = (0x1u << 16) + 0x2283u;  /* 22:   lw t0, 0(sp) */
  m[22] = (0x41u << 16) + 0x113u;  /* 23:   addi sp, sp, 4 */
  m[23] = (0x40a2u << 16) + 0x8533u;  /* 24:   sub a0, t0, a0 */
  m[24] = (0xa1u << 16) + 0x2023u;  /* 25:   sw a0, 0(sp) */
  m[25] = (0xffffu << 16) + 0xffffu;  /* 26:   call fact_rec */
  m[26] = (0x41u << 16) + 0x113u;  /* 27:   addi sp, sp, 4 */
  m[27] = (0x1u << 16) + 0x2283u;  /* 28:   lw t0, 0(sp) */
  m[28] = (0x41u << 16) + 0x113u;  /* 29:   addi sp, sp, 4 */
  m[29] = (0x2a2u << 16) + 0x8533u;  /* 30:   mul a0, t0, a0 */
  m[30] = (0x40u << 16) + 0x6fu;  /* 31:   j fact_rec_epilogue_0 */
  /* fact_rec_epilogue_0: */
  m[31] = (0x1u << 16) + 0x2083u;  /* 32:   lw ra, 0(sp) */
  m[32] = (0x41u << 16) + 0x2403u;  /* 33:   lw fp, 4(sp) */
  m[33] = (0x81u << 16) + 0x113u;  /* 34:   addi sp, sp, 8 */
  m[34] = (0x0u << 16) + 0x8067u;  /* 35:   ret */
  GC statistics
  Total allocations: 25177(words)
  Currently allocated: 25177(words)
  Current bank: 0


