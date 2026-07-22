
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'

$ echo 'int main(){return;}' > program.c
  $ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target parsetree
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
  
    return n * fact_rec(n - 1);
  }
  
  int fact_iter(int n) {
    int acc = 1;
  
    for (int i = 2; i <= n; i = i + 1) {
      acc = acc * i;
    }
  
    return acc;
  }
  
  int fact_rec(int n) {
    if ((n < 1)) {
    return 1;
  }
  
    return (n * fact_rec((n - 1)));
  }
  int fact_iter(int n) {
    int acc = 1;
  
    for (int i = 2;(i <= n);i = (i + 1))
  {
    acc = (acc * i);
  }
    return acc;
  }
  GC statistics
  Total allocations: 50674(words)
  Currently allocated: 50674(words)
  Current bank: 0








  $ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target rv64
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
  
    return n * fact_rec(n - 1);
  }
  
  int fact_iter(int n) {
    int acc = 1;
  
    for (int i = 2; i <= n; i = i + 1) {
      acc = acc * i;
    }
  
    return acc;
  }
  
  .global fact_rec
  .text
  fact_rec:
    addi sp, sp, -16
    sd fp, 8(sp)
    sd ra, 0(sp)
    mv fp, sp
  addi sp, sp, 0
    ld a0, 16(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 1
    ld t0, 0(sp)
    addi sp, sp, 8
    slt a0, t0, a0
    beqz a0, End_if_1
    li a0, 1
    j fact_rec_epilogue_0
  End_if_1: 
    ld a0, 16(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld a0, 16(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 1
    ld t0, 0(sp)
    addi sp, sp, 8
    sub a0, t0, a0
    sd a0, 0(sp)
    call fact_rec
    addi sp, sp, 8
    ld t0, 0(sp)
    addi sp, sp, 8
    mul a0, t0, a0
    j fact_rec_epilogue_0
  fact_rec_epilogue_0:
  addi sp, sp, 0
    ld ra, 0(sp)
    ld fp, 8(sp)
    addi sp, sp, 16
    ret
  .global fact_iter
  .text
  fact_iter:
    addi sp, sp, -16
    sd fp, 8(sp)
    sd ra, 0(sp)
    mv fp, sp
  addi sp, sp, -16
    li a0, 1
    sd a0, -8(fp)
    li a0, 2
    sd a0, -16(fp)
  Loop_3:
    ld a0, -16(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, 16(fp)
    ld t0, 0(sp)
    addi sp, sp, 8
    slt a0, a0, t0
    xori a0, a0, 1
    beqz a0, End_loop_4
    ld a0, -8(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -16(fp)
    ld t0, 0(sp)
    addi sp, sp, 8
    mul a0, t0, a0
    sd a0, -8(fp)
    ld a0, -16(fp)
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 1
    ld t0, 0(sp)
    addi sp, sp, 8
    add a0, t0, a0
    sd a0, -16(fp)
    j Loop_3
  End_loop_4:
    ld a0, -8(fp)
    j fact_iter_epilogue_2
  fact_iter_epilogue_2:
  addi sp, sp, 16
    ld ra, 0(sp)
    ld fp, 8(sp)
    addi sp, sp, 16
    ret
  GC statistics
  Total allocations: 50598(words)
  Currently allocated: 50598(words)
  Current bank: 0





