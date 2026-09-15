


  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc gcd.c -o file.out --target parsetree
  int gcd(int a, int b) {
    if (a==0) {
      return b;
    }
    if (b==0)
      return a;
    if (b<a)
      return gcd(a-b, b);
    return gcd(a, b-a);
  }
  
  GC statistics
  Total allocations: 31213(words)
  Currently allocated: 31213(words)
  Current bank: 0
  int gcd(int a, int b) {
    if ((a == 0)) {
    return b;
  }
  
    if ((b == 0)) return a;
  
    if ((b < a)) return gcd((a - b), b);
  
    return gcd(a, (b - a));
  }
  GC statistics
  Total allocations: 32537(words)
  Currently allocated: 32537(words)
  Current bank: 0












  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc gcd.c -o file.out --target rv64
  int gcd(int a, int b) {
    if (a==0) {
      return b;
    }
    if (b==0)
      return a;
    if (b<a)
      return gcd(a-b, b);
    return gcd(a, b-a);
  }
  
  GC statistics
  Total allocations: 31212(words)
  Currently allocated: 31212(words)
  Current bank: 0
  .global gcd
  .text
  #store has 53 instrs
  gcd:# 0
    addi sp, sp, -8# 1
    sw fp, 4(sp)# 2
    sw ra, 0(sp)# 3
    addi fp, sp, 0# 4
    lw a0, 8(fp)# 5
    bne a0, zero, .End_if_eq_1# 6
    lw a0, 12(fp)# 7
    j gcd_epilogue_0# 8
  .End_if_eq_1:# 9
    lw a0, 12(fp)# 10
    bne a0, zero, .End_if_eq_2# 11
    lw a0, 8(fp)# 12
    j gcd_epilogue_0# 13
  .End_if_eq_2:# 14
    lw a0, 12(fp)# 15
    add t0, a0, zero# 16
    lw a0, 8(fp)# 17
    bge t0, a0, .End_if_lt_3# 18
    addi sp, sp, -8# 19
    lw a0, 8(fp)# 20
    addi sp, sp, -4# 21
    sw a0, 0(sp)# 22
    lw a0, 12(fp)# 23
    lw t0, 0(sp)# 24
    addi sp, sp, 4# 25
    sub a0, t0, a0# 26
    sw a0, 4(sp)# 27
    lw a0, 12(fp)# 28
    sw a0, 0(sp)# 29
    call gcd# 30
    addi sp, sp, 8# 31
    j gcd_epilogue_0# 32
  .End_if_lt_3:# 33
    addi sp, sp, -8# 34
    lw a0, 8(fp)# 35
    sw a0, 4(sp)# 36
    lw a0, 12(fp)# 37
    addi sp, sp, -4# 38
    sw a0, 0(sp)# 39
    lw a0, 8(fp)# 40
    lw t0, 0(sp)# 41
    addi sp, sp, 4# 42
    sub a0, t0, a0# 43
    sw a0, 0(sp)# 44
    call gcd# 45
    addi sp, sp, 8# 46
    j gcd_epilogue_0# 47
  gcd_epilogue_0:# 48
    lw ra, 0(sp)# 49
    lw fp, 4(sp)# 50
    addi sp, sp, 8# 51
    ret# 52
  # gcd_epilogue_0 -> 44
  # .End_if_lt_3 -> 30
  # .End_if_eq_2 -> 12
  # .End_if_eq_1 -> 8
  # gcd -> 0
  /* gcd: */
  m[0] = (0xff81u << 16) + 0x113u;  /* 2:   addi sp, sp, -8 */
  m[1] = (0x81u << 16) + 0x2223u;  /* 3:   sw fp, 4(sp) */
  m[2] = (0x11u << 16) + 0x2023u;  /* 4:   sw ra, 0(sp) */
  m[3] = (0x1u << 16) + 0x413u;  /* 5:   addi fp, sp, 0 */
  m[4] = (0x84u << 16) + 0x2503u;  /* 6:   lw a0, 8(fp) */
  m[5] = (0x5u << 16) + 0x1663u;  /* 7:   bne a0, zero, .End_if_eq_1 */
  m[6] = (0xc4u << 16) + 0x2503u;  /* 8:   lw a0, 12(fp) */
  m[7] = (0x940u << 16) + 0x6fu;  /* 9:   j gcd_epilogue_0, offset = 37 */
  /* .End_if_eq_1: */
  m[8] = (0xc4u << 16) + 0x2503u;  /* 11:   lw a0, 12(fp) */
  m[9] = (0x5u << 16) + 0x1663u;  /* 12:   bne a0, zero, .End_if_eq_2 */
  m[10] = (0x84u << 16) + 0x2503u;  /* 13:   lw a0, 8(fp) */
  m[11] = (0x840u << 16) + 0x6fu;  /* 14:   j gcd_epilogue_0, offset = 33 */
  /* .End_if_eq_2: */
  m[12] = (0xc4u << 16) + 0x2503u;  /* 16:   lw a0, 12(fp) */
  m[13] = (0x5u << 16) + 0x2b3u;  /* 17:   add t0, a0, zero */
  m[14] = (0x84u << 16) + 0x2503u;  /* 18:   lw a0, 8(fp) */
  m[15] = (0x2a2u << 16) + 0xde63u;  /* 19:   bge t0, a0, .End_if_lt_3 */
  m[16] = (0xff81u << 16) + 0x113u;  /* 20:   addi sp, sp, -8 */
  m[17] = (0x84u << 16) + 0x2503u;  /* 21:   lw a0, 8(fp) */
  m[18] = (0xffc1u << 16) + 0x113u;  /* 22:   addi sp, sp, -4 */
  m[19] = (0xa1u << 16) + 0x2023u;  /* 23:   sw a0, 0(sp) */
  m[20] = (0xc4u << 16) + 0x2503u;  /* 24:   lw a0, 12(fp) */
  m[21] = (0x1u << 16) + 0x2283u;  /* 25:   lw t0, 0(sp) */
  m[22] = (0x41u << 16) + 0x113u;  /* 26:   addi sp, sp, 4 */
  m[23] = (0x40a2u << 16) + 0x8533u;  /* 27:   sub a0, t0, a0 */
  m[24] = (0xa1u << 16) + 0x2223u;  /* 28:   sw a0, 4(sp) */
  m[25] = (0xc4u << 16) + 0x2503u;  /* 29:   lw a0, 12(fp) */
  m[26] = (0xa1u << 16) + 0x2023u;  /* 30:   sw a0, 0(sp) */
  m[27] = (0xf95fu << 16) + 0xf0efu;  /* 31:   call gcd, offset = -27 */
  m[28] = (0x81u << 16) + 0x113u;  /* 32:   addi sp, sp, 8 */
  m[29] = (0x3c0u << 16) + 0x6fu;  /* 33:   j gcd_epilogue_0, offset = 15 */
  /* .End_if_lt_3: */
  m[30] = (0xff81u << 16) + 0x113u;  /* 35:   addi sp, sp, -8 */
  m[31] = (0x84u << 16) + 0x2503u;  /* 36:   lw a0, 8(fp) */
  m[32] = (0xa1u << 16) + 0x2223u;  /* 37:   sw a0, 4(sp) */
  m[33] = (0xc4u << 16) + 0x2503u;  /* 38:   lw a0, 12(fp) */
  m[34] = (0xffc1u << 16) + 0x113u;  /* 39:   addi sp, sp, -4 */
  m[35] = (0xa1u << 16) + 0x2023u;  /* 40:   sw a0, 0(sp) */
  m[36] = (0x84u << 16) + 0x2503u;  /* 41:   lw a0, 8(fp) */
  m[37] = (0x1u << 16) + 0x2283u;  /* 42:   lw t0, 0(sp) */
  m[38] = (0x41u << 16) + 0x113u;  /* 43:   addi sp, sp, 4 */
  m[39] = (0x40a2u << 16) + 0x8533u;  /* 44:   sub a0, t0, a0 */
  m[40] = (0xa1u << 16) + 0x2023u;  /* 45:   sw a0, 0(sp) */
  m[41] = (0xf5dfu << 16) + 0xf0efu;  /* 46:   call gcd, offset = -41 */
  m[42] = (0x81u << 16) + 0x113u;  /* 47:   addi sp, sp, 8 */
  m[43] = (0x40u << 16) + 0x6fu;  /* 48:   j gcd_epilogue_0, offset = 1 */
  /* gcd_epilogue_0: */
  m[44] = (0x1u << 16) + 0x2083u;  /* 50:   lw ra, 0(sp) */
  m[45] = (0x41u << 16) + 0x2403u;  /* 51:   lw fp, 4(sp) */
  m[46] = (0x81u << 16) + 0x113u;  /* 52:   addi sp, sp, 8 */
  m[47] = (0x0u << 16) + 0x8067u;  /* 53:   ret */
  GC statistics
  Total allocations: 46332(words)
  Currently allocated: 46332(words)
  Current bank: 0

