open Ast
open Sttfadk
open Environ
module CType = Compile_type
module CTerm = Compile_term
module CProof = Compile_proof
module Term = Kernel.Term
module Entry = Parsers.Entry
module Dpp = Api.Pp.Default

let compile_declaration dkenv name ty =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      let ty' = CType.compile_type dkenv empty_env a in
      Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      let ty' = CType.compile__type dkenv empty_env a in
      Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      let te' = CTerm.compile_term dkenv empty_env a in
      Axiom (of_name name, te')
  | _ ->
      if Sttfadk.is_tyop ty then TypeDecl (of_name name, arity_of_tyop ty)
      else assert false

let compile_definition dkenv name ty term =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      Definition
        ( of_name name,
          CType.compile_type dkenv empty_env a,
          CTerm.compile_term dkenv empty_env term )
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      (* The statement written and the one we get from the proof are beta,delta convertible. *)
      let j, proof = CProof.compile_proof dkenv empty_env term in
      let a' = CTerm.compile_term dkenv empty_env a in
      let proof' =
        if j.thm = a' then proof
        else
          Conv
            ( { j with thm = a' },
              proof,
              Sttfatyping.Tracer.annotate dkenv empty_env j.thm a' )
      in
      Theorem (of_name name, CTerm.compile_term dkenv empty_env a, proof')
  | _ ->
      if is_tyop ty then
        let vars, ty = CType.compile_type_definition dkenv empty_env term in
        TypeDef (of_name name, vars, ty)
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
  | Def _ ->
      Logs.err (fun m -> m "Definition without types are not supported");
      exit 1
  | Rules _ ->
      Logs.err (fun m -> m "Rules are not part of the sttforall logic");
      exit 1
  | Eval _ | Check _ | Infer _ | Print _ | DTree _ | Require _ | Name _ ->
      Logs.err (fun m -> m "Dedukti Commands are not supported");
      exit 1
