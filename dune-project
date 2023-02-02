(lang dune 2.8)

(cram enable)

(generate_opam_files true)

(license LGPL-3.0-only)

(authors "Kakadu@pm.me")

(maintainers "Kakadu")

(bug_reports "FIXME")

(homepage "FIXME")

(package
 (name MiniML)
 (synopsis "A MiniML interpreter")
 (description
  "FIXME. A longer description, for example, which are the most interesing features being supported, etc.")
 (version 0.1)
 (depends
  dune
  angstrom
  (ppx_inline_test :with-test)
  ppx_expect
  ppx_show
  (odoc :with-doc)
  (ocamlformat :build)
  base))
