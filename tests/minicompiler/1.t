
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

$ echo 'int main(){return;}' > program.c
  $ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target parsetree
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14336(words)
  Currently allocated: 14336(words)
  Current bank: 0
  int fact_rec(int n) {
    if ((n < 1)) {
    return 1;
  }
  
    return (n * fact_rec((n - 1)));
  }
  GC statistics
  Total allocations: 15044(words)
  Currently allocated: 15044(words)
  Current bank: 0








  $ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target rv64
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 14335(words)
  Currently allocated: 14335(words)
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
  /* fact_rec: */
  m[0] = (0xff81u << 16) + 0x113u;  /* 2:   addi sp, sp, -8 */
  m[1] = (0x81u << 16) + 0x2223u;  /* 3:   sw fp, 4(sp) */
  m[2] = (0x11u << 16) + 0x2023u;  /* 4:   sw ra, 0(sp) */
  m[3] = (0x1u << 16) + 0x413u;  /* 5:   addi fp, sp, 0 */
  m[4] = (0x84u << 16) + 0x2503u;  /* 6:   lw a0, 8(fp) */
  m[5] = (0x5u << 16) + 0x2b3u;  /* 7:   add t0, a0, zero */
  m[6] = (0x10u << 16) + 0x513u;  /* 8:   li a0, 1 */
  m[7] = (0xa2u << 16) + 0xd663u;  /* 9:   bge t0, a0, .End_if_lt_1 */
  m[8] = (0x10u << 16) + 0x513u;  /* 10:   li a0, 1 */
  m[9] = (0x4c0u << 16) + 0x6fu;  /* 11:   j fact_rec_epilogue_0, offset = 19 */
  /* .End_if_lt_1: */
  m[10] = (0x84u << 16) + 0x2503u;  /* 13:   lw a0, 8(fp) */
  m[11] = (0xffc1u << 16) + 0x113u;  /* 14:   addi sp, sp, -4 */
  m[12] = (0xa1u << 16) + 0x2023u;  /* 15:   sw a0, 0(sp) */
  m[13] = (0xffc1u << 16) + 0x113u;  /* 16:   addi sp, sp, -4 */
  m[14] = (0x84u << 16) + 0x2503u;  /* 17:   lw a0, 8(fp) */
  m[15] = (0xffc1u << 16) + 0x113u;  /* 18:   addi sp, sp, -4 */
  m[16] = (0xa1u << 16) + 0x2023u;  /* 19:   sw a0, 0(sp) */
  m[17] = (0x10u << 16) + 0x513u;  /* 20:   li a0, 1 */
  m[18] = (0x1u << 16) + 0x2283u;  /* 21:   lw t0, 0(sp) */
  m[19] = (0x41u << 16) + 0x113u;  /* 22:   addi sp, sp, 4 */
  m[20] = (0x40a2u << 16) + 0x8533u;  /* 23:   sub a0, t0, a0 */
  m[21] = (0xa1u << 16) + 0x2023u;  /* 24:   sw a0, 0(sp) */
  m[22] = (0xfa9fu << 16) + 0xf0efu;  /* 25:   call fact_rec, offset = -22 */
  m[23] = (0x41u << 16) + 0x113u;  /* 26:   addi sp, sp, 4 */
  m[24] = (0x1u << 16) + 0x2283u;  /* 27:   lw t0, 0(sp) */
  m[25] = (0x41u << 16) + 0x113u;  /* 28:   addi sp, sp, 4 */
  m[26] = (0x2a2u << 16) + 0x8533u;  /* 29:   mul a0, t0, a0 */
  m[27] = (0x40u << 16) + 0x6fu;  /* 30:   j fact_rec_epilogue_0, offset = 1 */
  /* fact_rec_epilogue_0: */
  m[28] = (0x1u << 16) + 0x2083u;  /* 32:   lw ra, 0(sp) */
  m[29] = (0x41u << 16) + 0x2403u;  /* 33:   lw fp, 4(sp) */
  m[30] = (0x81u << 16) + 0x113u;  /* 34:   addi sp, sp, 8 */
  m[31] = (0x0u << 16) + 0x8067u;  /* 35:   ret */
  GC statistics
  Total allocations: 24096(words)
  Currently allocated: 24096(words)
  Current bank: 0





