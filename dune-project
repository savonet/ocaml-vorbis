(lang dune 2.8)
(version 0.8.1)
(name vorbis)
(source (github savonet/ocaml-vorbis))
(license GPL-2.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name vorbis)
 (synopsis "Bindings to libvorbis")
 (depends
  conf-libvorbis
  conf-pkg-config
  (ocaml (>= 4.03.0))
  dune
  dune-configurator
  (ogg (>= 0.7.1)))
)
