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

ifdef VERBOSE
VERBOSE_FLAG = --display verbose
endif

ifdef WATCH
WATCH_FLAG = -w
endif

testsuite:
ifdef CLEAN
	git clean -fxdq testsuite/artifacts
	rm -rf testsuite/expected/*.out
endif
	dune b testsuite $(VERBOSE_FLAG)
	dune b testsuite @testsuite/cram $(VERBOSE_FLAG) $(WATCH_FLAG)

deps:
	opam install --depext-only .
	opam install --deps-only --with-doc .

TEST_COV_D ?= /tmp/rukaml
coverage:
	if [ -d $(TEST_COV_D) ]; then $(RM) -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	dune b @default @testsuite/all
	BISECT_FILE=$(TEST_COV_D)/GT dune b @runtest @testsuite/runtest --no-print-directory \
			--instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D) #--expect src/
	bisect-ppx-report summary --coverage-path $(TEST_COV_D) #--expect src/
