type t = Coq | Matita | Pvs | Lean | Hollight | OpenTheory

module type EXP = sig
  val print_ast : out_channel -> Api.Env.t -> Ast.ast -> unit
end

let exporter sys : (module EXP) =
  match sys with
  | Coq -> (module Coq)
  | Matita -> (module Matita)
  | Pvs -> (module Pvs)
  | Lean -> (module Lean)
  | Hollight -> (module Hollight)
  | OpenTheory -> (module Opentheory)
