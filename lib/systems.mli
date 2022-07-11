(** Definition of the available systems. A system is a syntax that can be read
    by some proof assistant. *)

type system = Coq | Matita | Pvs | Lean | Hollight | OpenTheory

(** Signature of an exporter for a system. Supporting a new system amounts to
    provide an implementation to EXP for the new system. *)
module type EXP = sig
  val print_ast : out_channel -> Api.Env.t -> Ast.ast -> unit
  (** [print_ast oc env ast] prints STTfa [ast] to out channel [oc] in Dedukti
      environment [env]. It performs side effects or fails. *)
end

val exporter : system -> (module EXP)
(** [exporter sys] maps systems to their export module. *)

val export :
  ?path:string list ->
  ?oc:out_channel ->
  system ->
  string ->
  (unit, unit) result
(** [export ?path ?oc sys files] exports content of files [files] to system
    [sys], writing on channel [oc] (which defaults to the standard output).
    Argument [path] specifies the include path for Dedukti. *)
