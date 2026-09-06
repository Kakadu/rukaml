
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
  
  GC statistics
  Total allocations: 32126(words)
  Currently allocated: 32126(words)
  Current bank: 0
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
  Total allocations: 33904(words)
  Currently allocated: 33904(words)
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
  
  GC statistics
  Total allocations: 32125(words)
  Currently allocated: 32125(words)
  Current bank: 0
  GC statistics
  Total allocations: 32125(words)
  Currently allocated: 32125(words)
  Current bank: 0





