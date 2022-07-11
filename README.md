STTfaXport
==========

STTfaXport allows to export theories written in Dedukti in the encoding of
STTfa to several proof assistants. The code has been extracted from
[logipedia](https://github.com/Deducteam/logipedia).

Requirements
------------

- ocaml (probably greater than 4.08)
- dedukti >= 2.7

API
---

The main API is given in `lib/systems.mli`.

Architecture
------------

The library defines an abstract syntax tree in `lib/ast.ml` for STTfa terms.
The first step of the process is to translate Dedukti files in the encoding of
STTfa to abstract syntax trees. Then for any system `S` available (they are
defined in `lib/systems.mli`), an exporter can be obtained using
`Systems.exporter S`. Such an exporter is a module with a function `print_ast`.
