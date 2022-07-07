type 'a eq = 'a -> 'a -> bool
(** Type of equality function. *)

type 'a pp = Format.formatter -> 'a -> unit
(** Type of a pretty printer. *)

module StrSet = Set.Make (String)

module Option = struct
  type 'a t = 'a option

  let map : ('a -> 'b) -> 'a t -> 'b t =
   fun f op -> match op with None -> None | Some v -> Some (f v)

  let get : 'a t -> 'a = function
    | Some v -> v
    | None -> invalid_arg "Option.get"
end

module List = struct
  include List

  (** [mem_eq eq e l] is [List.mem e l] with equality function [eq]. *)
  let rec mem_eq : 'a eq -> 'a -> 'a list -> bool =
   fun eq elt l ->
    match l with
    | [] -> false
    | x :: _ when eq x elt -> true
    | _ :: l -> mem_eq eq elt l
end

module Unix = struct
  include Unix

  (** [mkdir_rec s p] is like [mkdir -p s] setting file permissions [p]. *)
  let mkdir_rec : string -> file_perm -> unit =
   fun pth perm ->
    let reps = String.split_on_char '/' pth in
    let rec loop rem dir =
      match rem with
      | [] -> Unix.mkdir dir perm
      | p :: tl ->
          if not (Sys.file_exists dir) then Unix.mkdir dir perm;
          loop tl (Filename.concat dir p)
    in
    if Filename.is_relative pth then loop reps "." else loop reps "/"
end

module String = struct
  include String

  let drop : t -> int -> t =
   fun s start ->
    let len = length s in
    if start >= len then invalid_arg "String.drop"
    else sub s (start + 1) (len - start - 1)
end

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
