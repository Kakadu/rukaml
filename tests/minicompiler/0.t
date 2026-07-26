
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'
  $ ls
  compiler.anf.ml
  compiler.exe
  program0.c
$ chmod +w program.c
$ echo 'int main(){return;}' > program.c
$ cp program0.c program.c
$ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target parsetree




  $ qemu-riscv64 $FLAGS ./compiler.exe program0.c -o file.out --target rv64
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }
  
    return n * fact_rec(n - 1);
  }
  
  
  GC statistics
  Total allocations: 13033(words)
  Currently allocated: 13033(words)
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
  fact_rec:
    addi sp, sp, -8
    sd fp, 4(sp)
    sd ra, 0(sp)
    addi fp, sp, 0
    addi sp, sp, 0
    ld a0, 8(fp)
    addi sp, sp, -4
    sd a0, 0(sp)
    li a0, 1
    ld t0, 0(sp)
    addi sp, sp, 4
    slt a0, t0, a0
    beqz a0, End_if_1
    li a0, 1
    j fact_rec_epilogue_0
  End_if_1:
    ld a0, 8(fp)
    addi sp, sp, -4
    sd a0, 0(sp)
    addi sp, sp, -4
    ld a0, 8(fp)
    addi sp, sp, -4
    sd a0, 0(sp)
    li a0, 1
    ld t0, 0(sp)
    addi sp, sp, 4
    sub a0, t0, a0
    sd a0, 0(sp)
    call fact_rec
    addi sp, sp, 4
    ld t0, 0(sp)
    addi sp, sp, 4
    mul a0, t0, a0
    j fact_rec_epilogue_0
  fact_rec_epilogue_0:
    addi sp, sp, 0
    ld ra, 0(sp)
    ld fp, 4(sp)
    addi sp, sp, 8
    ret
  GC statistics
  Total allocations: 17880(words)
  Currently allocated: 17880(words)
  Current bank: 0
