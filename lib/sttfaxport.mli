(** Definition of the available systems. A system is a syntax that can be read
    by some proof assistant. *)

type system = Coq | Matita | Pvs | Lean | Hollight | OpenTheory

val export :
  ?path:string list ->
  ?oc:out_channel ->
  system ->
  string ->
  (unit, unit) result
(** [export ?path ?oc sys files] exports content of files [files] to system
    [sys], writing on channel [oc] (which defaults to the standard output).
    Argument [path] specifies the include path for Dedukti. *)
