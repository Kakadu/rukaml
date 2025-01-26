.PHONY: debs deps
all:
	dune build

test:
	dune runtest

watch:
	dune runtest -w

promote:
	dune runtest --auto-promote
clean:
	dune clean

DEBS += nasm clang-16 gcc-13 pkg-config
DEBS += gcc-13-riscv64-linux-gnu libc6-dev-riscv64-cross qemu-user binutils-riscv64-linux-gnu

debs:
	sudo apt install --yes --no-install-recommends $(DEBS)

deps: debs
	opam install --confirm-level=yes \
		dune-site angstrom.0.16.0 ppx_blob ppx_show ppx_expect llvm.16.0.6+nnp ctypes-foreign

