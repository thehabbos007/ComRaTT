(lang dune 3.7)

(name ComRaTT)

(generate_opam_files true)

(source
 (github username/reponame))

(authors
  "Ahmad Sattar Atta"
  "Marcus Sebastian Emil Homgaard"
)

(maintainers
  "Ahmad Sattar Atta"
  "Marcus Sebastian Emil Homgaard"
)

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name ComRaTT)
 (synopsis "A preliminary compiled Async RaTT implementation")
 (description "A longer description")
 (depends ocaml dune ppx_expect ppx_deriving ounit2 menhir base asai)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
