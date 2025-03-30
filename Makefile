.PHONY: debs deps clean
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
DEBS += gcc-13-riscv64-linux-gnu libc6-dev-riscv64-cross qemu-user binutils-riscv64-linux-gnu libcunit1-dev

debs:
	sudo apt install --yes --no-install-recommends $(DEBS)

deps: debs
	opam install --confirm-level=yes \
		dune-site angstrom.0.16.0 ppx_blob ppx_deriving.6.0.3 ppx_expect llvm.16.0.6+nnp ctypes-foreign

.PHONY: coverage
TEST_COV_D ?= /tmp/rukaml
coverage:
	if [ -d $(TEST_COV_D) ]; then $(RM) -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/GT dune runtest --no-print-directory \
			--instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D) #--expect src/
	bisect-ppx-report summary --coverage-path $(TEST_COV_D) #--expect src/