(** Compilation of Dedukti entries to STTfa AST items. *)

type error = [ `CompileDefnWithoutType | `CompileCommand | `CompileRewriteRule ]

val compile_entry :
  Api.Env.t ->
  Parsers.Entry.entry ->
  (Ast.item, [> error | Compile_proof.error | Compile_term.error ]) result
(** [compile_entry env entry] compiles Dedukti [entry] to an STTfa ast item
    into Dedukti environment [env]. *)
