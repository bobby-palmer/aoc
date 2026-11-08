(lang dune 3.0)

(generate_opam_files true)

(package
 (name aoc)
 (allow_empty)
 (depends
  (ocaml (>= 5.0))
  dune
  base
  stdio
  ppx_expect
  ppx_sexp_conv))
