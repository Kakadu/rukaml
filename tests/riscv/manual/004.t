$ ls
  $ riscv64-linux-gnu-as -march=rv64imac -o HelloWorld.o 004fac.s
  $ riscv64-linux-gnu-ld -o HelloWorld HelloWorld.o
  $ qemu-riscv64-static ./HelloWorld
  factorial is:
  [24]
