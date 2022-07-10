(** Definition of the available systems. A system is a syntax that can be read
    by some proof assistant. *)

type t = Coq | Matita | Pvs | Lean | Hollight | OpenTheory

(** Signature of an exporter for a system. Supporting a new system amounts to
    provide an implementation to EXP for the new system. *)
module type EXP = sig
  val print_ast : out_channel -> Api.Env.t -> Ast.ast -> unit
  (** [print_ast oc env ast] prints STTfa [ast] to out channel [oc] in Dedukti
      environment [env]. It performs side effects or fails. *)
end

val exporter : t -> (module EXP)
(** [exporter sys] maps systems to their export module. *)
