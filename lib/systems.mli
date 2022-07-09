type t = Coq | Matita | Pvs | Lean | Hollight | OpenTheory

module type EXP = sig
  val print_ast : out_channel -> Api.Env.t -> Ast.ast -> unit
end

val exporter : t -> (module EXP)
