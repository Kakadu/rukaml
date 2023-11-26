.PHONY: deps
all:
	dune build

test:
	dune runtest

watch:
	dune runtest -w

promote:
	dune runtest --auto-promote

deps:
	sudo apt install --yes --no-install-recommends \
		nasm clang-14 gcc-12 pkg-config \
		qemu-riscv64-static binutils-riscv64-linux-gnu
	opam install --yes \
		dune-site angstrom ppx_blob ppx_show ppx_expect llvm.14.0.6 ctypes-foreign

