(lang dune 3.16)

(name fp_lab2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Khoroshev Maksim")

(maintainers "Khoroshev Maksim")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name fp_lab2)
 (synopsis "ITMO FP course laboratory work #2")
 (description "s")
 (depends ocaml dune (alcotest :with-test) (junit_alcotest :with-test))
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
