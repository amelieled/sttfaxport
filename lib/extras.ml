type 'a eq = 'a -> 'a -> bool
(** Type of equality function. *)

type 'a pp = Format.formatter -> 'a -> unit
(** Type of a pretty printer. *)

module StrSet = Set.Make (String)

(** Some handy Dedukti functions or aliases. *)
module DkTools = struct
  module Mident = struct
    open Kernel.Basic

    type t = mident

    let equal : t eq = mident_eq

    let compare : t -> t -> int =
     fun m n -> String.compare (string_of_mident m) (string_of_mident n)

    let pp : t pp = Api.Pp.Default.print_mident
  end

  module MdSet = Kernel.Basic.MidentSet
  (** Functional sets of module identifiers. *)

  type entry = Parsers.Entry.entry

  let id_of_entry : Parsers.Entry.entry -> Kernel.Basic.ident =
    let open Parsers.Entry in
    function
    | Decl (_, id, _, _, _) -> id
    | Def (_, id, _, _, _, _) -> id
    | Rules (_, r :: _) -> (
        match r.pat with
        | Pattern (_, n, _) -> Kernel.Basic.id n
        | _ -> assert false (* the head of a rule should be a symbol *))
    | _ -> assert false
  (* tx_of_entry should be defined for more than Def, Decl and Rules *)

  let is_static :
      Kernel.Signature.t -> Kernel.Basic.loc -> Kernel.Basic.name -> bool =
   fun sign l n ->
    Kernel.Signature.get_staticity sign l n = Kernel.Signature.Static
end

module NameHashtbl = Hashtbl.Make (struct
  type t = Kernel.Basic.name

  let equal = Kernel.Basic.name_eq
  let hash = Hashtbl.hash
end)

module NameMap = Map.Make (struct
  type t = Kernel.Basic.name

  let compare : t -> t -> int = Stdlib.compare
end)
