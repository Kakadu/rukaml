

$ chmod +w program.c
  $ ls ../..
  run_rv32
  tests
  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc mc91.c -o file.out --target parsetree
  int m91(int n) {
    if (100 < n) {
      return n-10;
    }
    return m91(m91(n + 11));
  }
  
  
  GC statistics
  Total allocations: 16371(words)
  Currently allocated: 16371(words)
  Current bank: 0
  int m91(int n) {
    if ((100 < n)) {
    return (n - 10);
  }
  
    return m91(m91((n + 11)));
  }
  GC statistics
  Total allocations: 17105(words)
  Currently allocated: 17105(words)
  Current bank: 0










  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc mc91.c -o file.out --target rv64
  int m91(int n) {
    if (100 < n) {
      return n-10;
    }
    return m91(m91(n + 11));
  }
  
  
  GC statistics
  Total allocations: 16370(words)
  Currently allocated: 16370(words)
  Current bank: 0
  .global m91
  .text
  #store has 38 instrs
  m91:# 0
    addi sp, sp, -8# 1
    sw fp, 4(sp)# 2
    sw ra, 0(sp)# 3
    addi fp, sp, 0# 4
    lw a0, 8(fp)# 5
    li t0, 100# 6
    bge t0, a0, .End_if_lt_1# 7
    lw a0, 8(fp)# 8
    addi sp, sp, -4# 9
    sw a0, 0(sp)# 10
    li a0, 10# 11
    lw t0, 0(sp)# 12
    addi sp, sp, 4# 13
    sub a0, t0, a0# 14
    j m91_epilogue_0# 15
  .End_if_lt_1:# 16
    addi sp, sp, -4# 17
    addi sp, sp, -4# 18
    lw a0, 8(fp)# 19
    addi sp, sp, -4# 20
    sw a0, 0(sp)# 21
    li a0, 11# 22
    lw t0, 0(sp)# 23
    addi sp, sp, 4# 24
    add a0, t0, a0# 25
    sw a0, 0(sp)# 26
    call m91# 27
    addi sp, sp, 4# 28
    sw a0, 0(sp)# 29
    call m91# 30
    addi sp, sp, 4# 31
    j m91_epilogue_0# 32
  m91_epilogue_0:# 33
    lw ra, 0(sp)# 34
    lw fp, 4(sp)# 35
    addi sp, sp, 8# 36
    ret# 37
  # m91_epilogue_0 -> 31
  # .End_if_lt_1 -> 15
  # m91 -> 0
  /* m91: */
  m[0] = (0xff81u << 16) + 0x113u;  /* 2:   addi sp, sp, -8 */
  m[1] = (0x81u << 16) + 0x2223u;  /* 3:   sw fp, 4(sp) */
  m[2] = (0x11u << 16) + 0x2023u;  /* 4:   sw ra, 0(sp) */
  m[3] = (0x1u << 16) + 0x413u;  /* 5:   addi fp, sp, 0 */
  m[4] = (0x84u << 16) + 0x2503u;  /* 6:   lw a0, 8(fp) */
  m[5] = (0x640u << 16) + 0x293u;  /* 7:   li t0, 100 */
  m[6] = (0x2a2u << 16) + 0xd263u;  /* 8:   bge t0, a0, .End_if_lt_1 */
  m[7] = (0x84u << 16) + 0x2503u;  /* 9:   lw a0, 8(fp) */
  m[8] = (0xffc1u << 16) + 0x113u;  /* 10:   addi sp, sp, -4 */
  m[9] = (0xa1u << 16) + 0x2023u;  /* 11:   sw a0, 0(sp) */
  m[10] = (0xa0u << 16) + 0x513u;  /* 12:   li a0, 10 */
  m[11] = (0x1u << 16) + 0x2283u;  /* 13:   lw t0, 0(sp) */
  m[12] = (0x41u << 16) + 0x113u;  /* 14:   addi sp, sp, 4 */
  m[13] = (0x40a2u << 16) + 0x8533u;  /* 15:   sub a0, t0, a0 */
  m[14] = (0x440u << 16) + 0x6fu;  /* 16:   j m91_epilogue_0, offset = 17 */
  /* .End_if_lt_1: */
  m[15] = (0xffc1u << 16) + 0x113u;  /* 18:   addi sp, sp, -4 */
  m[16] = (0xffc1u << 16) + 0x113u;  /* 19:   addi sp, sp, -4 */
  m[17] = (0x84u << 16) + 0x2503u;  /* 20:   lw a0, 8(fp) */
  m[18] = (0xffc1u << 16) + 0x113u;  /* 21:   addi sp, sp, -4 */
  m[19] = (0xa1u << 16) + 0x2023u;  /* 22:   sw a0, 0(sp) */
  m[20] = (0xb0u << 16) + 0x513u;  /* 23:   li a0, 11 */
  m[21] = (0x1u << 16) + 0x2283u;  /* 24:   lw t0, 0(sp) */
  m[22] = (0x41u << 16) + 0x113u;  /* 25:   addi sp, sp, 4 */
  m[23] = (0xa2u << 16) + 0x8533u;  /* 26:   add a0, t0, a0 */
  m[24] = (0xa1u << 16) + 0x2023u;  /* 27:   sw a0, 0(sp) */
  m[25] = (0xf9dfu << 16) + 0xf0efu;  /* 28:   call m91, offset = -25 */
  m[26] = (0x41u << 16) + 0x113u;  /* 29:   addi sp, sp, 4 */
  m[27] = (0xa1u << 16) + 0x2023u;  /* 30:   sw a0, 0(sp) */
  m[28] = (0xf91fu << 16) + 0xf0efu;  /* 31:   call m91, offset = -28 */
  m[29] = (0x41u << 16) + 0x113u;  /* 32:   addi sp, sp, 4 */
  m[30] = (0x40u << 16) + 0x6fu;  /* 33:   j m91_epilogue_0, offset = 1 */
  /* m91_epilogue_0: */
  m[31] = (0x1u << 16) + 0x2083u;  /* 35:   lw ra, 0(sp) */
  m[32] = (0x41u << 16) + 0x2403u;  /* 36:   lw fp, 4(sp) */
  m[33] = (0x81u << 16) + 0x113u;  /* 37:   addi sp, sp, 8 */
  m[34] = (0x0u << 16) + 0x8067u;  /* 38:   ret */
  GC statistics
  Total allocations: 26866(words)
  Currently allocated: 26866(words)
  Current bank: 0

