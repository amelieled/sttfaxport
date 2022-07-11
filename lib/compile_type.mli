open Kernel

val compile__type : Api.Env.t -> Environ.env -> Term.term -> Ast._ty
val compile_type : Api.Env.t -> Environ.env -> Term.term -> Ast.ty
val compile_wrapped__type : Api.Env.t -> Environ.env -> Term.term -> Ast._ty
val compile_wrapped_type : Api.Env.t -> Environ.env -> Term.term -> Ast.ty

type error = [ `CompileTermUnhandled of Term.term | `CompileTermUntypedLambda ]

val compile__term :
  Api.Env.t -> Environ.env -> Term.term -> (Ast._te, [> error ]) result

val compile_term :
  Api.Env.t -> Environ.env -> Term.term -> (Ast.te, [> error ]) result

val compile_wrapped__term :
  Api.Env.t -> Environ.env -> Term.term -> (Ast._te, [> error ]) result

val compile_wrapped_term :
  Api.Env.t -> Environ.env -> Term.term -> (Ast.te, [> error ]) result

val compile_type_definition :
  Api.Env.t -> Environ.env -> Term.term -> Ast.ty_var list * Ast._ty
