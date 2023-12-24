.PHONY: debs deps 
all:
	dune build

test:
	dune runtest

watch:
	dune runtest -w

promote:
	dune runtest --auto-promote

DEBS += nasm clang-14 gcc-12 pkg-config
DEBS += gcc-riscv64-linux-gnu libc6-dev-riscv64-cross qemu-user binutils-riscv64-linux-gnu

debs:
	sudo apt install --yes --no-install-recommends $(DEBS)

deps: debs
	opam install --confirm-level=yes \
		dune-site angstrom ppx_blob ppx_show ppx_expect llvm.14.0.6 ctypes-foreign

