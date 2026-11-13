(lang dune 3.7)

(generate_opam_files true)

(package
 (name aoc)
 (allow_empty)
 (depends
  (ocaml (>= 5.0))
  dune
  core
  ppx_jane
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))
