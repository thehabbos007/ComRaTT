(lang dune 3.7)

(name ComRaTT)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name ComRaTT)
 (synopsis "A compiled Async RaTT implementation")
 (description "A longer description")
 (depends ocaml dune ppx_deriving ounit2 menhir)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
