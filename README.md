# [STTfaXport](https://github.com/Deducteam/sttfaxport)

Copyright Deducteam <dedukti-dev@inria.fr> 2022

With STTfaXport, you can export theories written in Dedukti in the encoding of
STTfa to other proof assistants.

To build STTfaXport, you need

- OCaml (probably greater than 4.08)
- [Dedukti](https://github.com/Deducteam/Dedukti) 2.7
- dune

You are free to copy, modify or redistribute STTfaXport with attribution under
the terms of the CeCILL-B license.

## Install

```command
$ dune install
```

## Getting started

The following guide shows you how to export a single file to Coq.

1. Download the file `connectives.dk` at
   <https://raw.githubusercontent.com/Deducteam/sttfaxport/87442e6d81398a3dff18787a59ffa0bb38ebb39c/test/connectives.dk>

2. Download the encoding of STTfa
   <https://raw.githubusercontent.com/Deducteam/sttfaxport/87442e6d81398a3dff18787a59ffa0bb38ebb39c/test/sttfa.dk>

2. Generate the object file of the encoding:
   ```command
   dk check -e sttfa.dk
   ```

2. Using a toplevel (`ocaml` or `utop`), print the translation of
   `connectives.dk` to Coq on the standard output:
   ```ocaml
   # #use "topfind";; (* Not necessary if you're using utop *)
   # #require "sttfaxport";;
   # Sttfaxport.Systems.(export Coq "connectives.dk")
   Parameter True : Prop.
   Parameter False : Prop.
   Parameter Not : Prop -> Prop.
   Parameter And : Prop -> Prop -> Prop.
   Parameter Or : Prop -> Prop -> Prop.
   Parameter ex : forall (A:Type), (A -> Prop) -> Prop.
   Parameter equal : forall (A:Type), A -> A -> Prop.
   Axiom I : True.
   Axiom falsity : forall (t:Prop), False -> t.
   Axiom nmk : forall (A:Prop), (A -> False) -> Not A.
   Axiom Not_ind : forall (A:Prop), forall (Q:Prop), ((A -> False) -> Q) -> (Not A) -> Q.
   Axiom conj : forall (A:Prop), forall (B:Prop), A -> B -> And A B.
   Axiom match_And_prop : forall (A:Prop), forall (B:Prop), forall (return_:Prop), (A -> B -> return_) -> (And A B) -> return_.
   Axiom or_introl : forall (A:Prop), forall (B:Prop), A -> Or A B.
   Axiom or_intror : forall (A:Prop), forall (B:Prop), B -> Or A B.
   Axiom match_Or_prop : forall (A:Prop), forall (B:Prop), forall (return_:Prop), (A -> return_) -> (B -> return_) -> (Or A B) -> return_.
   Axiom ex_intro : forall A, forall (P:(A -> Prop)), forall (x:A), (P x) -> ex (A) P.
   Axiom match_ex_prop : forall A, forall (P:(A -> Prop)), forall (return_:Prop), (forall (x:A), (P x) -> return_) -> (ex (A) P) -> return_.
   Axiom refl_equal : forall A, forall (x:A), equal (A) x x.
   Axiom equal_leibniz : forall A, forall (x:A), forall (y:A), (equal (A) x y) -> forall (P:(A -> Prop)), (P x) -> P y.
   - : (unit, unit) result = Ok ()
   ```

## What to do next

The file `HACKING.md` gives some instructions for developers.

You can report bugs on the [issue tracker](https://github.com/Deducteam/sttfaxport/issues).

You can contact the developers at <dedukti-dev@inria.fr>
