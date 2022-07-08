type 'a eq = 'a -> 'a -> bool
(** Type of equality function. *)

type 'a pp = Format.formatter -> 'a -> unit
(** Type of a pretty printer. *)

module StrSet = Set.Make (String)

(** Some handy Dedukti functions or aliases. *)
module DkTools = struct
  let is_static :
      Kernel.Signature.t -> Kernel.Basic.loc -> Kernel.Basic.name -> bool =
   fun sign l n ->
    Kernel.Signature.get_staticity sign l n = Kernel.Signature.Static
end
