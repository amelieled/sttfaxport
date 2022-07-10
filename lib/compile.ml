open Extras
open Ast
open Sttfadk
open Environ
module CType = Compile_type
module CTerm = Compile_term
module CProof = Compile_proof
module Term = Kernel.Term
module Entry = Parsers.Entry
module Dpp = Api.Pp.Default
open Result.Monad

type error = [ `CompileDefnWithoutType | `CompileRewriteRule | `CompileCommand ]

let compile_declaration dkenv name ty =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      let ty' = CType.compile_type dkenv empty_env a in
      return @@ Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      let ty' = CType.compile__type dkenv empty_env a in
      return @@ Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      let* te' = CTerm.compile_term dkenv empty_env a in
      return @@ Axiom (of_name name, te')
  | _ ->
      if Sttfadk.is_tyop ty then
        return @@ TypeDecl (of_name name, arity_of_tyop ty)
      else assert false

let compile_definition dkenv name ty term =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      let* te = CTerm.compile_term dkenv empty_env term in
      return
      @@ Definition (of_name name, CType.compile_type dkenv empty_env a, te)
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      (* The statement written and the one we get from the proof are beta,delta convertible. *)
      let* j, proof = CProof.compile_proof dkenv empty_env term in
      let* a' = CTerm.compile_term dkenv empty_env a in
      let* proof' =
        if j.thm = a' then return proof
        else
          let* trace = Sttfatyping.Tracer.annotate dkenv empty_env j.thm a' in
          return @@ Conv ({ j with thm = a' }, proof, trace)
      in
      let* te = CTerm.compile_term dkenv empty_env a in
      return @@ Theorem (of_name name, te, proof')
  | _ ->
      if is_tyop ty then
        let vars, ty = CType.compile_type_definition dkenv empty_env term in
        return @@ TypeDef (of_name name, vars, ty)
      else assert false

let compile_entry env entry =
  let md = Api.Env.get_name env in
  match entry with
  | Parsers.Entry.Decl (l, id, sc, st, ty) ->
      Api.Env.declare env l id sc st ty;
      Logs.debug (fun m -> m "Compiling decl \"%a\"" Dpp.print_ident id);
      compile_declaration env (Basic.mk_name md id) ty
  | Def (l, id, sc, f, Some ty, te) ->
      Api.Env.define env l id sc f te (Some ty);
      Logs.debug (fun m -> m "Compiling def \"%a\"" Dpp.print_ident id);
      compile_definition env (Basic.mk_name md id) ty te
  | Def _ -> Error `CompileDefnWithoutType
  | Rules _ -> Error `CompileRewriteRule
  | Eval _ | Check _ | Infer _ | Print _ | DTree _ | Require _ | Name _ ->
      Error `CompileCommand
