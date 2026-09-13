

$ chmod +w program.c

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
  Total allocations: 31204(words)
  Currently allocated: 31204(words)
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
  Total allocations: 32528(words)
  Currently allocated: 32528(words)
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
  Total allocations: 31203(words)
  Currently allocated: 31203(words)
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
  m[0] = (0x81u << 16) + 0x2223u;  /* 1:   addi sp, sp, -8 */
  m[1] = (0x11u << 16) + 0x2023u;  /* 2:   sw fp, 4(sp) */
  m[2] = (0x1u << 16) + 0x413u;  /* 3:   sw ra, 0(sp) */
  m[3] = (0x84u << 16) + 0x2503u;  /* 4:   addi fp, sp, 0 */
  m[4] = (0x5u << 16) + 0x1663u;  /* 5:   lw a0, 8(fp) */
  m[5] = (0xc4u << 16) + 0x2503u;  /* 6:   bne a0, zero, .End_if_eq_1 */
  m[6] = (0x240u << 16) + 0x6fu;  /* 7:   lw a0, 12(fp) */
  m[7] = (0xc4u << 16) + 0x2503u;  /* 8:   j gcd_epilogue_0 */
  /* .End_if_eq_1: */
  m[8] = (0x84u << 16) + 0x2503u;  /* 9:   lw a0, 12(fp) */
  m[9] = (0x1e0u << 16) + 0x6fu;  /* 10:   bne a0, zero, .End_if_eq_2 */
  m[10] = (0xc4u << 16) + 0x2503u;  /* 11:   lw a0, 8(fp) */
  m[11] = (0x5u << 16) + 0x2b3u;  /* 12:   j gcd_epilogue_0 */
  /* .End_if_eq_2: */
  m[12] = (0x2a2u << 16) + 0xde63u;  /* 13:   lw a0, 12(fp) */
  m[13] = (0xff81u << 16) + 0x113u;  /* 14:   add t0, a0, zero */
  m[14] = (0x84u << 16) + 0x2503u;  /* 15:   lw a0, 8(fp) */
  m[15] = (0xffc1u << 16) + 0x113u;  /* 16:   bge t0, a0, .End_if_lt_3 */
  m[16] = (0xa1u << 16) + 0x2023u;  /* 17:   addi sp, sp, -8 */
  m[17] = (0xc4u << 16) + 0x2503u;  /* 18:   lw a0, 8(fp) */
  m[18] = (0x1u << 16) + 0x2283u;  /* 19:   addi sp, sp, -4 */
  m[19] = (0x41u << 16) + 0x113u;  /* 20:   sw a0, 0(sp) */
  m[20] = (0x40a2u << 16) + 0x8533u;  /* 21:   lw a0, 12(fp) */
  m[21] = (0xa1u << 16) + 0x2223u;  /* 22:   lw t0, 0(sp) */
  m[22] = (0xc4u << 16) + 0x2503u;  /* 23:   addi sp, sp, 4 */
  m[23] = (0xa1u << 16) + 0x2023u;  /* 24:   sub a0, t0, a0 */
  m[24] = (0xf95fu << 16) + 0xf0efu;  /* 25:   sw a0, 4(sp) */
  m[25] = (0x81u << 16) + 0x113u;  /* 26:   lw a0, 12(fp) */
  m[26] = (0xc0u << 16) + 0x6fu;  /* 27:   sw a0, 0(sp) */
  m[27] = (0xff81u << 16) + 0x113u;  /* 28:   call gcd */
  m[28] = (0x84u << 16) + 0x2503u;  /* 29:   addi sp, sp, 8 */
  m[29] = (0xa1u << 16) + 0x2223u;  /* 30:   j gcd_epilogue_0 */
  /* .End_if_lt_3: */
  m[30] = (0xffc1u << 16) + 0x113u;  /* 31:   addi sp, sp, -8 */
  m[31] = (0xa1u << 16) + 0x2023u;  /* 32:   lw a0, 8(fp) */
  m[32] = (0x84u << 16) + 0x2503u;  /* 33:   sw a0, 4(sp) */
  m[33] = (0x1u << 16) + 0x2283u;  /* 34:   lw a0, 12(fp) */
  m[34] = (0x41u << 16) + 0x113u;  /* 35:   addi sp, sp, -4 */
  m[35] = (0x40a2u << 16) + 0x8533u;  /* 36:   sw a0, 0(sp) */
  m[36] = (0xa1u << 16) + 0x2023u;  /* 37:   lw a0, 8(fp) */
  m[37] = (0xfd9fu << 16) + 0xf0efu;  /* 38:   lw t0, 0(sp) */
  m[38] = (0x81u << 16) + 0x113u;  /* 39:   addi sp, sp, 4 */
  m[39] = (0xc0u << 16) + 0x6fu;  /* 40:   sub a0, t0, a0 */
  m[40] = (0x1u << 16) + 0x2083u;  /* 41:   sw a0, 0(sp) */
  m[41] = (0x41u << 16) + 0x2403u;  /* 42:   call gcd */
  m[42] = (0x81u << 16) + 0x113u;  /* 43:   addi sp, sp, 8 */
  m[43] = (0x0u << 16) + 0x8067u;  /* 44:   j gcd_epilogue_0 */
  /* gcd_epilogue_0: */
  m[44] = (0x0u << 16) + 0x0u;  /* 45:   lw ra, 0(sp) */
  m[45] = (0x0u << 16) + 0x0u;  /* 46:   lw fp, 4(sp) */
  m[46] = (0x0u << 16) + 0x0u;  /* 47:   addi sp, sp, 8 */
  m[47] = (0x0u << 16) + 0x0u;  /* 48:   ret */
  GC statistics
  Total allocations: 46209(words)
  Currently allocated: 46209(words)
  Current bank: 0
