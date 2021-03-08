(lang dune 2.0)
(version 0.8.0)
(name vorbis)
(source (github savonet/ocaml-vorbis))
(license GPL-2.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name vorbis)
 (synopsis "Bindings to libvorbis")
 (depends
  conf-libvorbis
  conf-pkg-config
  (ocaml (>= 4.03.0))
  (dune (>= 2.0))
  dune-configurator
  (ogg (>= 0.7.0)))
)
