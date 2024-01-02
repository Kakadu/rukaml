$ ls
$ file $(readlink bm1.exe)
$ riscv64-linux-gnu-objdump -d bm1.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./bm1.exe  
  24
  3
