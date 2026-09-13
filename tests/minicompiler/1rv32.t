

  $ chmod +w program.c

  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program.c -o file.out --target parsetree
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14565(words)
  Currently allocated: 14565(words)
  Current bank: 0
  int fact_rec(int n) {
    if ((n < 1)) {
    return 1;
  }
  
    return (n * fact_rec((n - 1)));
  }
  GC statistics
  Total allocations: 15273(words)
  Currently allocated: 15273(words)
  Current bank: 0









  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program.c -o file.out --target rv64
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14564(words)
  Currently allocated: 14564(words)
  Current bank: 0
  .global fact_rec
  .text
  #store has 35 instrs
  fact_rec:# 0
    addi sp, sp, -8# 1
    sw fp, 4(sp)# 2
    sw ra, 0(sp)# 3
    addi fp, sp, 0# 4
    lw a0, 8(fp)# 5
    add t0, a0, zero# 6
    li a0, 1# 7
    bge t0, a0, .End_if_lt_1# 8
    li a0, 1# 9
    j fact_rec_epilogue_0# 10
  .End_if_lt_1:# 11
    lw a0, 8(fp)# 12
    addi sp, sp, -4# 13
    sw a0, 0(sp)# 14
    addi sp, sp, -4# 15
    lw a0, 8(fp)# 16
    addi sp, sp, -4# 17
    sw a0, 0(sp)# 18
    li a0, 1# 19
    lw t0, 0(sp)# 20
    addi sp, sp, 4# 21
    sub a0, t0, a0# 22
    sw a0, 0(sp)# 23
    call fact_rec# 24
    addi sp, sp, 4# 25
    lw t0, 0(sp)# 26
    addi sp, sp, 4# 27
    mul a0, t0, a0# 28
    j fact_rec_epilogue_0# 29
  fact_rec_epilogue_0:# 30
    lw ra, 0(sp)# 31
    lw fp, 4(sp)# 32
    addi sp, sp, 8# 33
    ret# 34
  # fact_rec_epilogue_0 -> 28
  # .End_if_lt_1 -> 10
  # fact_rec -> 0
  ==   bge t0, a0, .End_if_lt_1, offset = 3
  ==   call fact_rec, offset = -22
  /* fact_rec: */
  m[0] = (0x81u << 16) + 0x2223u;  /* 1:   addi sp, sp, -8 */
  m[1] = (0x11u << 16) + 0x2023u;  /* 2:   sw fp, 4(sp) */
  m[2] = (0x1u << 16) + 0x413u;  /* 3:   sw ra, 0(sp) */
  m[3] = (0x84u << 16) + 0x2503u;  /* 4:   addi fp, sp, 0 */
  m[4] = (0x5u << 16) + 0x2b3u;  /* 5:   lw a0, 8(fp) */
  m[5] = (0x10u << 16) + 0x513u;  /* 6:   add t0, a0, zero */
  m[6] = (0xffffu << 16) + 0xffffu;  /* 7:   li a0, 1 */
  m[7] = (0x10u << 16) + 0x513u;  /* 8:   bge t0, a0, .End_if_lt_1 */
  m[8] = (0x4c0u << 16) + 0x6fu;  /* 9:   li a0, 1 */
  m[9] = (0x84u << 16) + 0x2503u;  /* 10:   j fact_rec_epilogue_0 */
  /* .End_if_lt_1: */
  m[10] = (0xa1u << 16) + 0x2023u;  /* 11:   lw a0, 8(fp) */
  m[11] = (0xffc1u << 16) + 0x113u;  /* 12:   addi sp, sp, -4 */
  m[12] = (0x84u << 16) + 0x2503u;  /* 13:   sw a0, 0(sp) */
  m[13] = (0xffc1u << 16) + 0x113u;  /* 14:   addi sp, sp, -4 */
  m[14] = (0xa1u << 16) + 0x2023u;  /* 15:   lw a0, 8(fp) */
  m[15] = (0x10u << 16) + 0x513u;  /* 16:   addi sp, sp, -4 */
  m[16] = (0x1u << 16) + 0x2283u;  /* 17:   sw a0, 0(sp) */
  m[17] = (0x41u << 16) + 0x113u;  /* 18:   li a0, 1 */
  m[18] = (0x40a2u << 16) + 0x8533u;  /* 19:   lw t0, 0(sp) */
  m[19] = (0xa1u << 16) + 0x2023u;  /* 20:   addi sp, sp, 4 */
  m[20] = (0xffffu << 16) + 0xffffu;  /* 21:   sub a0, t0, a0 */
  m[21] = (0x41u << 16) + 0x113u;  /* 22:   sw a0, 0(sp) */
  m[22] = (0x1u << 16) + 0x2283u;  /* 23:   call fact_rec */
  m[23] = (0x41u << 16) + 0x113u;  /* 24:   addi sp, sp, 4 */
  m[24] = (0x2a2u << 16) + 0x8533u;  /* 25:   lw t0, 0(sp) */
  m[25] = (0x40u << 16) + 0x6fu;  /* 26:   addi sp, sp, 4 */
  m[26] = (0x1u << 16) + 0x2083u;  /* 27:   mul a0, t0, a0 */
  m[27] = (0x41u << 16) + 0x2403u;  /* 28:   j fact_rec_epilogue_0 */
  /* fact_rec_epilogue_0: */
  m[28] = (0x0u << 16) + 0x8067u;  /* 29:   lw ra, 0(sp) */
  m[29] = (0x0u << 16) + 0x0u;  /* 30:   lw fp, 4(sp) */
  m[30] = (0x0u << 16) + 0x0u;  /* 31:   addi sp, sp, 8 */
  m[31] = (0x0u << 16) + 0x0u;  /* 32:   ret */
  GC statistics
  Total allocations: 24448(words)
  Currently allocated: 24448(words)
  Current bank: 0


