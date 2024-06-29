.PHONY: devdeps deps clean bench1

all:
	dune test

OPAM_PACKAGES := dune angstrom bisect_ppx ppx_expect ppx_inline_test ppx_deriving ppx_show       \
	      qcheck-core

devdeps: $(eval OPAM_PACKAGES += ocaml-lsp-server ocamlformat.0.26.2)
deps:
	opam install --confirm-level=unsafe-yes $(OPAM_PACKAGES)

clean:
	$(RM) -r _build _coverage
