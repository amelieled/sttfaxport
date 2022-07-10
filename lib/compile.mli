(** Compilation of Dedukti entries to STTfa AST items. *)

type error =
  [ `CompileProofUnhandled of Kernel.Term.term
  | `CompileProofUnhandledThm of Ast.te
  | `CompileTermUnhandled of Kernel.Term.term
  | `CompileTermUntypedLambda
  | `CompileDefnWithoutType
  | `CompileCommand
  | `CompileRewriteRule ]

val compile_entry :
  Api.Env.t -> Parsers.Entry.entry -> (Ast.item, [> error ]) result
(** [compile_entry env entry] compiles Dedukti [entry] to an STTfa ast item
    into Dedukti environment [env]. *)
