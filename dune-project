(lang dune 3.12)

(name aoc)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "James Meyers")
(maintainers "James Meyers")

(license MIT)

(package
 (name aoc)
 (synopsis "AoC 2023 Solutions")
 (description "My solutions for the 2023 Advent of Code challenge - I decided to solve in OCaml for fun and to refamiliarize myself with the language")
 (depends ocaml dune alcotest)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
