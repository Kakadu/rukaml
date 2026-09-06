
  $ export FLAGS='-L /usr/riscv64-linux-gnu -cpu max'
  $ chmod +w program.c

  $ $(cat ../../run_rv32) ./compiler.rv32.exe -dc program.c -o file.out --target parsetree
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
  Total allocations: 32259(words)
  Currently allocated: 32259(words)
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
  Total allocations: 34037(words)
  Currently allocated: 34037(words)
  Current bank: 0
