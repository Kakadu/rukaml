MAKEFLAGS += --always-make

build:
	dune build

test:
	dune runtest

watch:
	dune runtest -w

promote:
	dune runtest --auto-promote

clean:
	dune clean

testsuite:
ifdef CLEAN
	git clean -fxdq testsuite/artifacts
	rm -rf testsuite/expected/*.out
endif
ifdef VERBOSE
	dune b testsuite --display verbose
endif
	dune b testsuite
	dune runt testsuite

DEBS += nasm clang-16 gcc-13 pkg-config
DEBS += gcc-13-riscv64-linux-gnu libc6-dev-riscv64-cross qemu-user binutils-riscv64-linux-gnu libcunit1-dev

debs:
	sudo apt install --yes --no-install-recommends $(DEBS)

deps: debs
	opam install --deps-only --with-doc --with-dev-setup .

TEST_COV_D ?= /tmp/rukaml
coverage:
	if [ -d $(TEST_COV_D) ]; then $(RM) -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/GT dune runtest --no-print-directory \
			--instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D) #--expect src/
	bisect-ppx-report summary --coverage-path $(TEST_COV_D) #--expect src/
