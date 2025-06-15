(lang dune 3.6)

(using dune_site 0.1)

(cram enable)

(generate_opam_files true)

(license LGPL-3.0-only)

(authors "Kakadu@pm.me")

(maintainers "Kakadu")

(bug_reports "https://github.com/Kakadu/rukaml/issues")

(homepage "FIXME")

(name rukaml)

(package
 (name rukaml)
 (synopsis "A MiniML interpreter")
 (description
  "FIXME. A longer description, for example, which are the most interesing features being supported, etc.")
 (version 0.1)
 (sites
  (lib plugins))
 (depends
  dune
  dune-site
  (angstrom
   (= 0.16.0))
  (ocaml
   (or
    (= 4.14.2)
    (>= 5.3.0)))
  (ppx_inline_test :with-test)
  ppx_expect
  ppx_deriving
  bisect_ppx
  (llvm
   (>= 16.0.0))
  ctypes-foreign ; needed for LLVM
  ppx_blob
  (ocamlgraph
   (>= 2.1.0))
  (odoc :with-doc)
  (ocamlformat
   (and
    (= 0.27.0)
    :with-dev))
  base))
