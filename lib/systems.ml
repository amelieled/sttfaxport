type t = Coq | Matita | Pvs | Lean | Hollight | OpenTheory | Latex

module type EXP = sig
  val print_ast : out_channel -> Ast.ast -> unit
end

let exporter sys : (module EXP) =
        match sys with
        | Coq -> (module Coq)
        | Matita -> (module Matita)
        | Pvs -> (module Pvs)
        | _ -> assert false
