val export :
  ?path:string list ->
  ?oc:out_channel ->
  Systems.t ->
  string list ->
  (unit, unit) result
(** [export ?path ?oc sys files] exports content of files [files] to system
    [sys], writing on channel [oc] (which defaults to the standard output).
    Argument [path] specifies the include path for Dedukti. *)
