

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
  Total allocations: 16362(words)
  Currently allocated: 16362(words)
  Current bank: 0
  int m91(int n) {
    if ((100 < n)) {
    return (n - 10);
  }
  
    return m91(m91((n + 11)));
  }
  GC statistics
  Total allocations: 17096(words)
  Currently allocated: 17096(words)
  Current bank: 0










  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc mc91.c -o file.out --target rv64
  int m91(int n) {
    if (100 < n) {
      return n-10;
    }
    return m91(m91(n + 11));
  }
  
  
  GC statistics
  Total allocations: 16361(words)
  Currently allocated: 16361(words)
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
  m[0] = (0x81u << 16) + 0x2223u;  /* 1:   addi sp, sp, -8 */
  m[1] = (0x11u << 16) + 0x2023u;  /* 2:   sw fp, 4(sp) */
  m[2] = (0x1u << 16) + 0x413u;  /* 3:   sw ra, 0(sp) */
  m[3] = (0x84u << 16) + 0x2503u;  /* 4:   addi fp, sp, 0 */
  m[4] = (0x640u << 16) + 0x293u;  /* 5:   lw a0, 8(fp) */
  m[5] = (0x2a2u << 16) + 0xd263u;  /* 6:   li t0, 100 */
  m[6] = (0x84u << 16) + 0x2503u;  /* 7:   bge t0, a0, .End_if_lt_1 */
  m[7] = (0xffc1u << 16) + 0x113u;  /* 8:   lw a0, 8(fp) */
  m[8] = (0xa1u << 16) + 0x2023u;  /* 9:   addi sp, sp, -4 */
  m[9] = (0xa0u << 16) + 0x513u;  /* 10:   sw a0, 0(sp) */
  m[10] = (0x1u << 16) + 0x2283u;  /* 11:   li a0, 10 */
  m[11] = (0x41u << 16) + 0x113u;  /* 12:   lw t0, 0(sp) */
  m[12] = (0x40a2u << 16) + 0x8533u;  /* 13:   addi sp, sp, 4 */
  m[13] = (0x440u << 16) + 0x6fu;  /* 14:   sub a0, t0, a0 */
  m[14] = (0xffc1u << 16) + 0x113u;  /* 15:   j m91_epilogue_0 */
  /* .End_if_lt_1: */
  m[15] = (0x84u << 16) + 0x2503u;  /* 16:   addi sp, sp, -4 */
  m[16] = (0xffc1u << 16) + 0x113u;  /* 17:   addi sp, sp, -4 */
  m[17] = (0xa1u << 16) + 0x2023u;  /* 18:   lw a0, 8(fp) */
  m[18] = (0xb0u << 16) + 0x513u;  /* 19:   addi sp, sp, -4 */
  m[19] = (0x1u << 16) + 0x2283u;  /* 20:   sw a0, 0(sp) */
  m[20] = (0x41u << 16) + 0x113u;  /* 21:   li a0, 11 */
  m[21] = (0xa2u << 16) + 0x8533u;  /* 22:   lw t0, 0(sp) */
  m[22] = (0xa1u << 16) + 0x2023u;  /* 23:   addi sp, sp, 4 */
  m[23] = (0xf9dfu << 16) + 0xf0efu;  /* 24:   add a0, t0, a0 */
  m[24] = (0x41u << 16) + 0x113u;  /* 25:   sw a0, 0(sp) */
  m[25] = (0xa1u << 16) + 0x2023u;  /* 26:   call m91 */
  m[26] = (0xf91fu << 16) + 0xf0efu;  /* 27:   addi sp, sp, 4 */
  m[27] = (0x41u << 16) + 0x113u;  /* 28:   sw a0, 0(sp) */
  m[28] = (0x40u << 16) + 0x6fu;  /* 29:   call m91 */
  m[29] = (0x1u << 16) + 0x2083u;  /* 30:   addi sp, sp, 4 */
  m[30] = (0x41u << 16) + 0x2403u;  /* 31:   j m91_epilogue_0 */
  /* m91_epilogue_0: */
  m[31] = (0x0u << 16) + 0x8067u;  /* 32:   lw ra, 0(sp) */
  m[32] = (0x0u << 16) + 0x0u;  /* 33:   lw fp, 4(sp) */
  m[33] = (0x0u << 16) + 0x0u;  /* 34:   addi sp, sp, 8 */
  m[34] = (0x0u << 16) + 0x0u;  /* 35:   ret */
  GC statistics
  Total allocations: 26783(words)
  Currently allocated: 26783(words)
  Current bank: 0

