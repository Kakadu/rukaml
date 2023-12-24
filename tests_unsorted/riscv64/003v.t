$ ls
$ file $(readlink bm1.exe)
$ riscv64-linux-gnu-objdump -d bm1.exe
  $ qemu-riscv64 -cpu rv64,v=true,vlen=128,vext_spec=v1.0 ./strcpy.exe
  Hello World!
  12
  01234567890123456789
  20
  Hello World!
 
