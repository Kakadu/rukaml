# compiler.ml — компилятор который собирается с помощью rukaml
# program.cpp — программа которую собирает compiler

AMD64_STDLIB = ../../back/amd64/rukaml_stdlib.c

_build:
	mkdir -p _build

clean:
	rm -rf _build/*

# compiler.ml ~> compiler.s   (via rukaml)
compiler.s: _build
	dune exec -- driver compiler.ml --target amd64 -o _build/compiler.s	

# compiler.s ~> compiler.exe
compiler.exe: compiler.s
	yasm -f elf64 _build/compiler.s -o _build/compiler.o
	x86_64-linux-gnu-gcc --sysroot=/usr/x86_64-linux-gnu/sys-root/fc42 -static _build/compiler.o $(AMD64_STDLIB) -o _build/compiler.exe
	rm _build/compiler.o

# program.cpp ~> parsetree.cpp   (via compiler)
parsetree.cpp: compiler.exe program.cpp
	qemu-x86_64 _build/compiler.exe program.cpp -o ./_build/parsetree.cpp --target parsetree

# program.cpp ~> program.s   (via compiler)
program.s: compiler.exe program.cpp
	qemu-x86_64 _build/compiler.exe program.cpp -o ./_build/program.s --target rv64

# program.s ~> program.exe   
program.exe: program.s
	riscv64-linux-gnu-gcc --sysroot=/usr/riscv64-linux-gnu/sys-root/fc42 -static ./_build/program.s -o _build/program.exe

all: program.exe

.PHONY: run

run: program.exe
	qemu-riscv64 _build/program.exe; echo "program exit code: $$?"
