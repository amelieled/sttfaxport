# Developer guide

## Architecture

The library defines an abstract syntax tree in `lib/ast.ml` for STTfa terms.
The first step of the process is to translate Dedukti files in the encoding of
STTfa to abstract syntax trees. Then for any system `S` available (they are
defined in `lib/systems.mli`), an exporter can be obtained using
`Systems.exporter S`. Such an exporter is a module with a function `print_ast`.

The code has been extracted from
[logipedia](https://github.com/Deducteam/logipedia).

## How to add an exporter?

If you want to add a new exporter:

1. Write a new module that implements the interface `Sttfaxport.EXP`
1. Add a new constructor in the type `Sttfaxport.system`
2. Extend the function `Sttfaxport.exporter` from the new constructor to the
   new module
