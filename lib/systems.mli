type t = Coq | Matita | Pvs | Lean | Hollight | OpenTheory | Latex

module type EXP = sig
  val print_ast : out_channel -> Ast.ast -> unit
end

val exporter : t -> (module EXP)
