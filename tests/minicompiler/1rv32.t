

  $ chmod +w program.c

  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program.c -o file.out --target parsetree
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14456(words)
  Currently allocated: 14456(words)
  Current bank: 0
  int fact_rec(int n) {
    if ((n < 1)) {
    return 1;
  }
  
    return (n * fact_rec((n - 1)));
  }
  GC statistics
  Total allocations: 15164(words)
  Currently allocated: 15164(words)
  Current bank: 0









  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program.c -o file.out --target rv64
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14455(words)
  Currently allocated: 14455(words)
  Current bank: 0
  .global fact_rec
  .text
  #store has 39 instrs
  fact_rec:# 0
    addi sp, sp, -8# 1
    sw fp, 4(sp)# 2
    sw ra, 0(sp)# 3
    addi fp, sp, 0# 4
    lw a0, 8(fp)# 5
    addi sp, sp, -4# 6
    sw a0, 0(sp)# 7
    li a0, 1# 8
    lw t0, 0(sp)# 9
    addi sp, sp, 4# 10
    slt a0, t0, a0# 11
    beq a0, zero, End_if_1# 12
    li a0, 1# 13
    j fact_rec_epilogue_0# 14
  End_if_1:# 15
    lw a0, 8(fp)# 16
    addi sp, sp, -4# 17
    sw a0, 0(sp)# 18
    addi sp, sp, -4# 19
    lw a0, 8(fp)# 20
    addi sp, sp, -4# 21
    sw a0, 0(sp)# 22
    li a0, 1# 23
    lw t0, 0(sp)# 24
    addi sp, sp, 4# 25
    sub a0, t0, a0# 26
    sw a0, 0(sp)# 27
    call fact_rec# 28
    addi sp, sp, 4# 29
    lw t0, 0(sp)# 30
    addi sp, sp, 4# 31
    mul a0, t0, a0# 32
    j fact_rec_epilogue_0# 33
  fact_rec_epilogue_0:# 34
    lw ra, 0(sp)# 35
    lw fp, 4(sp)# 36
    addi sp, sp, 8# 37
    ret# 38
  # fact_rec_epilogue_0 -> 32
  # End_if_1 -> 14
  # fact_rec -> 0
  /* fact_rec: */
  m[0] = (0x81u << 16) + 0x2223u;  /* 1:   addi sp, sp, -8 */
  m[1] = (0x11u << 16) + 0x2023u;  /* 2:   sw fp, 4(sp) */
  m[2] = (0x1u << 16) + 0x413u;  /* 3:   sw ra, 0(sp) */
  m[3] = (0x84u << 16) + 0x2503u;  /* 4:   addi fp, sp, 0 */
  m[4] = (0xffc1u << 16) + 0x113u;  /* 5:   lw a0, 8(fp) */
  m[5] = (0xa1u << 16) + 0x2023u;  /* 6:   addi sp, sp, -4 */
  m[6] = (0x10u << 16) + 0x513u;  /* 7:   sw a0, 0(sp) */
  m[7] = (0x1u << 16) + 0x2283u;  /* 8:   li a0, 1 */
  m[8] = (0x41u << 16) + 0x113u;  /* 9:   lw t0, 0(sp) */
  m[9] = (0xa2u << 16) + 0xa533u;  /* 10:   addi sp, sp, 4 */
  m[10] = (0x5u << 16) + 0x663u;  /* 11:   slt a0, t0, a0 */
  m[11] = (0x10u << 16) + 0x513u;  /* 12:   beq a0, zero, End_if_1 */
  m[12] = (0x4c0u << 16) + 0x6fu;  /* 13:   li a0, 1 */
  m[13] = (0x84u << 16) + 0x2503u;  /* 14:   j fact_rec_epilogue_0 */
  /* End_if_1: */
  m[14] = (0xa1u << 16) + 0x2023u;  /* 15:   lw a0, 8(fp) */
  m[15] = (0xffc1u << 16) + 0x113u;  /* 16:   addi sp, sp, -4 */
  m[16] = (0x84u << 16) + 0x2503u;  /* 17:   sw a0, 0(sp) */
  m[17] = (0xffc1u << 16) + 0x113u;  /* 18:   addi sp, sp, -4 */
  m[18] = (0xa1u << 16) + 0x2023u;  /* 19:   lw a0, 8(fp) */
  m[19] = (0x10u << 16) + 0x513u;  /* 20:   addi sp, sp, -4 */
  m[20] = (0x1u << 16) + 0x2283u;  /* 21:   sw a0, 0(sp) */
  m[21] = (0x41u << 16) + 0x113u;  /* 22:   li a0, 1 */
  m[22] = (0x40a2u << 16) + 0x8533u;  /* 23:   lw t0, 0(sp) */
  m[23] = (0xa1u << 16) + 0x2023u;  /* 24:   addi sp, sp, 4 */
  m[24] = (0xf99fu << 16) + 0xf0efu;  /* 25:   sub a0, t0, a0 */
  m[25] = (0x41u << 16) + 0x113u;  /* 26:   sw a0, 0(sp) */
  m[26] = (0x1u << 16) + 0x2283u;  /* 27:   call fact_rec */
  m[27] = (0x41u << 16) + 0x113u;  /* 28:   addi sp, sp, 4 */
  m[28] = (0x2a2u << 16) + 0x8533u;  /* 29:   lw t0, 0(sp) */
  m[29] = (0x40u << 16) + 0x6fu;  /* 30:   addi sp, sp, 4 */
  m[30] = (0x1u << 16) + 0x2083u;  /* 31:   mul a0, t0, a0 */
  m[31] = (0x41u << 16) + 0x2403u;  /* 32:   j fact_rec_epilogue_0 */
  /* fact_rec_epilogue_0: */
  m[32] = (0x0u << 16) + 0x8067u;  /* 33:   lw ra, 0(sp) */
  m[33] = (0x0u << 16) + 0x0u;  /* 34:   lw fp, 4(sp) */
  m[34] = (0x0u << 16) + 0x0u;  /* 35:   addi sp, sp, 8 */
  m[35] = (0x0u << 16) + 0x0u;  /* 36:   ret */
  GC statistics
  Total allocations: 25400(words)
  Currently allocated: 25400(words)
  Current bank: 0


