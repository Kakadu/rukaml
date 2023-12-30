$ ls
$ file $(readlink bm1.exe)
$ riscv64-linux-gnu-objdump -d bm1.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64,v=true,vlen=128,vext_spec=v1.0 ./strcpy.exe
  24
  3
