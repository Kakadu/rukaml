(lang dune 3.19)

(generate_opam_files true)

(cram enable)

(license LGPL-3.0-only)

(source
 (github kakadu/rukaml))

(authors "Kakadu <kakadu@pm.me>")

(maintainers "Kakadu <kakadu@pm.me>")

(name rukaml)

(package
 (allow_empty)
 (name rukaml)
 (synopsis "Another ML compiler")
 (depends
  (ocaml
   (or
    (= 4.14.2)
    (>= 5.3.0)))
  dune
  dune-configurator
  base
  stdio
  ppx_deriving
  bisect_ppx_ng
  parsexp
  (angstrom
   (= 0.16.0))
  (llvm
   (>= 16.0.0))
  (ocamlgraph
   (>= 2.1.0))
  qcheck
  ppx_deriving_qcheck
  ppx_inline_test_nobase
  (ppx_sexp_conv :build)
  ;
  (odoc :with-doc)
  (ocamlformat
   (and
    (= 0.27.0)
    :with-dev-setup))
  (ocaml-lsp-server :with-dev-setup)
  (utop :with-dev-setup)))
