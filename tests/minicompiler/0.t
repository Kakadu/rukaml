
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'
$ chmod +w program.c
$ echo 'int main(){return;}' > program.c
  $ cp program0.c program.c
  $ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target parsetree
  int fact_rec(int n) {
    if (n < 1) {
      return 1;
    }

    return n * fact_rec(n - 1);
  }


  int fact_rec(int n) {
    if ((n < 1)) {
    return 1;
  }

    return (n * fact_rec((n - 1)));
  }
  GC statistics
  Total allocations: 20545(words)
  Currently allocated: 20545(words)
  Current bank: 0





$ qemu-riscv64 $FLAGS ./compiler.exe program.c -o file.out --target rv64
