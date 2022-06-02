(lang dune 2.0)

(name tea)

(documentation "https://sunshangpp.github.io/tea/")

(source
 (github sunshangpp/tea))

(license ISC)

(authors "Shangpeng Sun")

(maintainers "Shangpeng Sun")

(generate_opam_files true)

(package
 (name tea)
 (synopsis "TEA (the elm architecture) in the shell")
 (description
  "Building interactive shell applications with the elm architecture (https://guide.elm-lang.org/architecture/)")
 (depends
  (ocaml
   (>= 4.13.0))
  dune
  core
  async
  notty
  notty_async
  cohttp-async ; used by examples
  async_ssl ; used by examples
  (odoc :with-doc)))
