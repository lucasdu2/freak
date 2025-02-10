(lang dune 3.15)

(name freak)

(generate_opam_files true)

(source
 (github lucasdu2/freak))

(authors "Lucas Du")

(maintainers "Lucas Du")

(license MIT)

(package
 (name freak)
 (synopsis "Fuzzing for regex engines")
 (description "Fuzzing for regex engines")
 (depends ocaml dune)
 (tags
  ("fuzzer" "regex")))
