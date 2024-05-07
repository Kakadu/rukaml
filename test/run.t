$ cat > asdf.pas <<-EOF
> x=6;
> acc=1;
> while x>0 do acc=acc*x; x=x-1; done
> EOF
  $ cat > asdf.pas <<-EOF
  > x=6;
  > EOF
  $ cat asdf.pas
  x=6;
  $ ../lib/driver.exe asdf.pas -o out.s
  $ riscv64-linux-gnu-as -march=rv64gc_zbb out.s -o hello.o
  $ riscv64-linux-gnu-ld hello.o rv64_runtime.o -o program.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./program.exe
  x                              6
