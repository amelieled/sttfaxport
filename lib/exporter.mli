(** Functions to write files in a foreign system.  Available systems
    are the ones described by the type {!type:Systems.system}. *)

val export_to_system_as_string : Systems.t -> Api.Env.t -> Ast.item -> string
(** Directly exports from STTfa to the given system as a string *)

(* val get_sttfa_exporter : Systems.t -> (module Export.S) *)
(** Generate an Exporter module from STTfa to the given system.
    This allows to handle export through the Makefile system. *)
