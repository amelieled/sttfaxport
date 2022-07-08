(lang dune 3.3)

(name sttfaxport)

(generate_opam_files true)

(package
 (name sttfaxport)
 (synopsis "An exporter for files written in the STTfa logic.")
 (depends ocaml
          dune
          logs
          (dedukti (>= 2.7))))
