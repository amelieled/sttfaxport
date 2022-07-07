open Console
open Ast
open Sttfadk
open Environ
module CType = Compile_type
module CTerm = Compile_term
module CProof = Compile_proof
module Term = Kernel.Term
module Entry = Parsers.Entry

let log_sttfa = new_logger "stfc"
let log_sttfa = log_sttfa.logger

let compile_declaration dkenv name ty =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile_type dkenv empty_env a in
      Parameter (of_name name, ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      (* Format.eprintf "[COMPILE] parameter: %a@." Pp.print_name name ; *)
      let ty' = CType.compile__type dkenv empty_env a in
      Parameter (of_name name, Ty ty')
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eps cst ->
      (* Format.eprintf "[COMPILE] axiom: %a@." Pp.print_name name ; *)
      let te' = CTerm.compile_term dkenv empty_env a in
      Axiom (of_name name, te')
  | _ ->
      if Sttfadk.is_tyop ty then
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        TypeDecl (of_name name, arity_of_tyop ty)
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
        (* Format.eprintf "[COMPILE] typeop: %a@." Pp.print_name name ; *)
        let vars, ty = CType.compile_type_definition dkenv empty_env term in
        TypeDef (of_name name, vars, ty)
      else assert false

let compile_entry env entry =
  let module Pp = Api.Pp.Default in
  let md = Api.Env.get_name env in
  match entry with
  | Parsers.Entry.Decl (l, id, sc, st, ty) ->
      Api.Env.declare env l id sc st ty;
      log_sttfa ~lvl:6 "compiling decl [%a]" Pp.print_ident id;
      compile_declaration env (Basic.mk_name md id) ty
  | Def (l, id, sc, f, Some ty, te) ->
      Api.Env.define env l id sc f te (Some ty);
      log_sttfa ~lvl:6 "compiling def [%a]" Pp.print_ident id;
      compile_definition env (Basic.mk_name md id) ty te
  | Def _ -> exit_with "Definition without types are not supported"
  | Rules _ -> exit_with "Rules are not part of the sttforall logic"
  | _ -> exit_with "Dedukti Commands are not supported"
