all:
	dune build

test:
	dune runtest

watch:
	dune runtest -w

promote:
	dune runtest --auto-promote

deps:
	opam install --yes dune-site angstrom ppx_blob ppx_show llvm ppx_expect
	sudo apt install --yes --no-install-recommends \
		nasm clang-14

