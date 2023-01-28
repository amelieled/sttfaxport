open Kernel

val _infer : Api.Env.t -> Environ.env -> Ast._te -> Ast._ty

val subst :
  Api.Env.t ->
  Environ.env ->
  Ast.proof ->
  Term.term ->
  (Ast.te, [> Compile_type.error ]) result

val _eq : Environ.env -> Ast._te -> Ast._te -> bool
val eq : Environ.env -> Ast.te -> Ast.te -> bool
val _is_beta_normal : Api.Env.t -> Environ.env -> Ast._te -> bool
val is_beta_normal : Api.Env.t -> Environ.env -> Ast.te -> bool

module ComputeStrategy : sig
  val beta_only : Reduction.red_cfg
  val beta_one : Reduction.red_cfg
  val beta_steps : int -> Reduction.red_cfg
  val delta : Basic.name -> Reduction.red_cfg
  val delta_only : Basic.name -> Reduction.red_cfg
end
