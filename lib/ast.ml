(** This file defines a full AST of STTforall.
    Informations are redundant to facilitate exportation. *)

open Extras
module B = Kernel.Basic

(** {b NOTE} underscored types are monomorphic, not underscored are
    polymorphic. *)

type vtype
type vterm
type vhypothesis
type _ variable = V of string

let string_of_var (V x : _ variable) : string = x
let sov (V x : _ variable) : string = x

module type VARSET = sig
  type _ t

  val empty : _ t
  val add : 'a variable -> 'a t -> 'a t
  val remove : 'a variable -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val exists : ('a variable -> bool) -> 'a t -> bool
  val fold : ('a variable -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module VarSet : VARSET = struct
  module VSet = Set.Make (String)

  type _ t = VSet.t

  let empty : type a. a t = VSet.empty
  let add (V x : 'a variable) (set : 'a t) : 'a t = VSet.add x set
  let remove (V x : 'a variable) (set : 'a t) : 'a t = VSet.remove x set
  let union = VSet.union
  let exists f = VSet.exists (fun x -> f (V x))
  let fold f = VSet.fold (fun x -> f (V x))
end

let voi (type a) i : a variable = V (B.string_of_ident i)
let type_var : B.ident -> vtype variable = voi
let term_var : B.ident -> vterm variable = voi
let hypothesis_var : B.ident -> vhypothesis variable = voi

type name = string * string

type _ty =
  | TyVar of vtype variable
  | Arrow of _ty * _ty
  | TyOp of name * _ty list
  | Prop

type ty = ForallK of vtype variable * ty | Ty of _ty

type _te =
  | TeVar of vterm variable
  | Abs of vterm variable * _ty * _te
  | App of _te * _te
  | Forall of vterm variable * _ty * _te
  | Impl of _te * _te
  | AbsTy of vtype variable * _te
  | Cst of name * _ty list

type te = ForallP of vtype variable * te | Te of _te
type ty_ctx = vtype variable list
type te_ctx = (vterm variable * _ty) list

module TeSet = Set.Make (struct
  type t = string * _te

  let compare = compare
end)

type hyp = TeSet.t
type judgment = { ty : ty_ctx; te : te_ctx; hyp : hyp; thm : te }

type ctx =
  | CAbs
  | CAppL
  | CAppR
  | CForall
  | CImplL
  | CImplR
  | CAbsTy
  | CForallP

let print_ctx fmt = function
  | CAbs -> Format.fprintf fmt "CAbs"
  | CAppL -> Format.fprintf fmt "CAppL"
  | CAppR -> Format.fprintf fmt "CAppR"
  | CForall -> Format.fprintf fmt "CForall"
  | CImplL -> Format.fprintf fmt "CImplL"
  | CImplR -> Format.fprintf fmt "CImplR"
  | CAbsTy -> Format.fprintf fmt "CAbsTy"
  | CForallP -> Format.fprintf fmt "CForallP"

let print_ctxs fmt ctxs = B.pp_list "," print_ctx fmt ctxs

type redex = Delta of name * _ty list | Beta of _te

let print_redex oc r =
  match r with
  | Delta ((md, id), _tys) -> Format.fprintf oc "%s,%s" md id
  | Beta _ -> Format.fprintf oc "beta"

type rewrite_seq = (redex * ctx list) list
type trace = { left : rewrite_seq; right : rewrite_seq }

let print_rewrite_ctx oc (rw, ctxs) =
  Format.fprintf oc "unfold %a at %a;@." print_redex rw print_ctxs ctxs

let print_rewrite_seq oc rws = List.iter (print_rewrite_ctx oc) rws

let print_trace oc trace =
  Format.fprintf oc "left:@.%a@." print_rewrite_seq trace.left;
  Format.fprintf oc "right:@.%a@." print_rewrite_seq trace.right

type proof =
  | Assume of judgment * vhypothesis variable
  | Lemma of name * judgment
  | Conv of judgment * proof * trace
  | ImplE of judgment * proof * proof
  | ImplI of judgment * proof * vhypothesis variable
  | ForallE of judgment * proof * _te
  | ForallI of judgment * proof * vterm variable
  | ForallPE of judgment * proof * _ty
  | ForallPI of judgment * proof * vtype variable

type arity = int

type item =
  | Parameter of name * ty
  | Definition of name * ty * te
  | Axiom of name * te
  | Theorem of name * te * proof
  | TypeDecl of name * arity
  | TypeDef of name * vtype variable list * _ty

type kind =
  [ `Parameter | `Definition | `Axiom | `Theorem | `TypeDecl | `TypeDef ]

type ast = { md : string; dep : StrSet.t; items : item list }

let kind_of = function
  | Parameter _ -> `Parameter
  | Definition _ -> `Definition
  | Axiom _ -> `Axiom
  | Theorem _ -> `Theorem
  | TypeDecl _ -> `TypeDecl
  | TypeDef _ -> `TypeDef

let name_of = function
  | Parameter (name, _)
  | Definition (name, _, _)
  | Axiom (name, _)
  | Theorem (name, _, _)
  | TypeDecl (name, _)
  | TypeDef (name, _, _) ->
      name

let judgment_of = function
  | Assume (j, _) -> j
  | Lemma (_, j) -> j
  | Conv (j, _, _) -> j
  | ImplE (j, _, _) -> j
  | ImplI (j, _, _) -> j
  | ForallE (j, _, _) -> j
  | ForallI (j, _, _) -> j
  | ForallPE (j, _, _) -> j
  | ForallPI (j, _, _) -> j
