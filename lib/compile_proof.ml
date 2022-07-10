open Extras
open Ast
open Sttfadk
open Environ
module CType = Compile_type
module CTerm = Compile_term
open Result.Monad

type error =
  [ `CompileproofUnhandled of Term.term | `CompileproofUnhandledThm of te ]

let make_judgment env hyp thm = { ty = env.ty; te = env.te; hyp; thm }
let extract_te te = match te with Te _te -> _te | _ -> assert false

let rec compile_proof dkenv env proof : (_, [> error ]) result =
  match proof with
  | Term.DB (_, _, n) ->
      let var = get_dk_var env n in
      let te' = List.assoc var env.prf in
      let j = make_judgment env (TeSet.of_list env.prf) (Te te') in
      return (j, Assume (j, var))
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
      let id = gen_fresh env [] id in
      let* jp, proof = compile_proof dkenv (add_ty_var_dk env id) _te in
      let j = make_judgment env jp.hyp (ForallP (soi id, jp.thm)) in
      return (j, ForallPI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _ty), _te)
    when is_sttfa_const sttfa_etap cst || is_sttfa_const sttfa_eta cst ->
      let _ty' = CType.compile_wrapped__type dkenv env _ty in
      let id = gen_fresh env [] id in
      let* jp, proof = compile_proof dkenv (add_te_var_dk env id _ty') _te in
      let j =
        make_judgment env jp.hyp (Te (Forall (soi id, _ty', extract_te jp.thm)))
      in
      return (j, ForallI (j, proof, soi id))
  | Term.Lam (_, id, Some (Term.App (cst, _, _) as _te), prf)
    when is_sttfa_const sttfa_eps cst ->
      let remove_hyp _ =
        TeSet.filter (fun (id', _) ->
            if string_of_ident id = id' then false else true)
      in
      let* _te' = CTerm.compile_wrapped__term dkenv env _te in
      let* jp, proof =
        compile_proof dkenv (add_prf_ctx env (string_of_ident id) _te _te') prf
      in
      let j =
        make_judgment env (remove_hyp _te' jp.hyp)
          (Te (Impl (_te', extract_te jp.thm)))
      in
      return (j, ImplI (j, proof, string_of_ident id))
  | Term.Const (lc, name) ->
      let te = Api.Env.get_type dkenv lc name in
      let* te' = CTerm.compile_wrapped_term dkenv empty_env te in
      let j = make_judgment env (TeSet.of_list env.prf) te' in
      return (j, Lemma (of_name name, j))
  | Term.App (f, a, args) ->
      let* j, f' = compile_proof dkenv env f in
      List.fold_left
        (fun jf a ->
          let* j, f' = jf in
          compile_arg dkenv env j f' a)
        (return (j, f'))
        (a :: args)
  | x -> Error (`CompileproofUnhandled x)

and compile_arg dkenv env j f' a : (_, [> error ]) result =
  let* te = Sttfatyping.subst dkenv env f' a in
  let j' = { j with thm = te } in
  let* j, f' = get_product dkenv env j f' in
  match j.thm with
  | ForallP _ ->
      let a' = CType.compile__type dkenv env a in
      return (j', ForallPE (j', f', a'))
  | Te (Forall _) ->
      let* a' = CTerm.compile__term dkenv env a in
      let proof = ForallE (j', f', a') in
      let* rws, after = Sttfatyping.Tracer.annotate_beta dkenv env j'.thm in
      let trace = { left = rws; right = [] } in
      let j' = { j with thm = after } in
      return (j', Conv (j', proof, trace))
  | Te (Impl (p', q')) ->
      let* ja, a' = compile_proof dkenv env a in
      let _te = match ja.thm with Te _te -> _te | _ -> assert false in
      let inferred = Impl (p', q') in
      let expected = Impl (_te, q') in
      if Sttfatyping._eq env inferred expected then
        return (j', ImplE (j', f', a'))
      else
        let* trace = Sttfatyping.Tracer._annotate dkenv env inferred expected in
        let f' = Conv ({ j with thm = Te expected }, f', trace) in
        return (j', ImplE (j', f', a'))
  | Te _ as thm -> Error (`CompileproofUnhandledThm thm)

and get_product dkenv env j f' : (_, [> error ]) result =
  match j.thm with
  | ForallP _ | Te (Forall _) | Te (Impl _) -> return (j, f')
  | Te tyfl ->
      let _, ctx, redex = Sttfatyping.Tracer.get_app_redex dkenv true [] tyfl in
      let* tyfr = Sttfatyping.Tracer._reduce dkenv env ctx redex tyfl in
      let* trace = Sttfatyping.Tracer._annotate dkenv env tyfl tyfr in
      let j' = { j with thm = Te tyfr } in
      let proof' = Conv (j', f', trace) in
      get_product dkenv env j' proof'
