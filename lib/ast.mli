(** This file defines a full AST of STTforall.
    Informations are redundant to facilitate exportation. *)

open Extras
module B = Kernel.Basic

(** {b NOTE} underscored types are monomorphic, not underscored are
    polymorphic. *)

(** {1 Variables} *)

type _ variable
(** The type of variables. *)

type vtype
type vterm
type vhypothesis

val string_of_var : _ variable -> string

val sov : _ variable -> string
(** [sov v] returns a string made from variable [v]. *)

module type VARSET = sig
  type _ t
  (** A set of variables *)

  val empty : _ t
  val add : 'a variable -> 'a t -> 'a t
  val remove : 'a variable -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val exists : ('a variable -> bool) -> 'a t -> bool
  val fold : ('a variable -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module VarSet : VARSET

(** {2 Constructors for variables} *)

val type_var : B.ident -> vtype variable
val term_var : B.ident -> vterm variable
val hypothesis_var : B.ident -> vhypothesis variable

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

module TeSet : Set.S with type elt = string * _te

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

val print_ctx : ctx printer
val print_ctxs : ctx list printer

type redex = Delta of name * _ty list | Beta of _te

val print_redex : redex printer

type rewrite_seq = (redex * ctx list) list
type trace = { left : rewrite_seq; right : rewrite_seq }

val print_rewrite_ctx : (redex * ctx list) printer
val print_rewrite_seq : (redex * ctx list) list printer
val print_trace : trace printer

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
type mdeps = (string * StrSet.t) list

val kind_of : item -> kind
val name_of : item -> name
val judgment_of : proof -> judgment
