  $ riscv64-linux-gnu-as -march=rv64imac -o HelloWorld.o 003hello.s
  $ riscv64-linux-gnu-ld -o HelloWorld HelloWorld.o
  $ qemu-riscv64-static ./HelloWorld
  Hello World!
