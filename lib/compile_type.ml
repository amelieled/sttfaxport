open Extras
open Ast
open Sttfadk
open Environ
module Dpp = Api.Pp.Default

(** {1 Types} *)

let compile_tyop tyop =
  match tyop with Term.Const (_, name) -> of_name name | _ -> assert false

let rec compile__type env _ty =
  match _ty with
  | Term.Const (_, _) when is_sttfa_const sttfa_prop _ty -> Prop
  | Term.App (c, left, [ right ]) when is_sttfa_const sttfa_arrow c ->
      let left' = compile__type env left in
      let right' = compile__type env right in
      Arrow (left', right')
  | Term.DB (_, _, n) -> TyVar (type_var (get_dk_var env n))
  | Term.App (tyop, a, args) ->
      let tyop' = compile_tyop tyop in
      let args' = List.map (fun x -> compile__type env x) (a :: args) in
      TyOp (tyop', args')
  | Term.Const _ -> TyOp (compile_tyop _ty, [])
  | Kind | Type _ | Lam _ | Pi _ -> assert false

let compile__type dkenv env _ty =
  let _ty =
    Api.Env.reduction dkenv ~ctx:env.dk
      ~red:{ Kernel.Reduction.default_cfg with target = Kernel.Reduction.Snf }
      _ty
  in
  compile__type env _ty

let rec compile_type (dkenv : Api.Env.t) (env : env) ty =
  match ty with
  | Term.App (c, Term.Lam (_, var, _, ty), [])
    when is_sttfa_const sttfa_forall_kind_type c ->
      let var = gen_fresh env var in
      let ty' = compile_type dkenv (add_ty_var_dk env var) ty in
      ForallK (type_var var, ty')
  | Term.App (c, a, []) when is_sttfa_const sttfa_p c ->
      Ty (compile__type dkenv env a)
  | _ -> assert false

let compile_wrapped__type dkenv env (ty : Term.term) =
  match ty with
  | Term.App (cst, Term.App (c, a, []), [])
    when is_sttfa_const sttfa_etap cst && is_sttfa_const sttfa_p c ->
      compile__type dkenv env a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      compile__type dkenv env a
  | _ -> assert false

let compile_wrapped_type dkenv env (ty : Term.term) =
  match ty with
  | Term.App (cst, a, []) when is_sttfa_const sttfa_etap cst ->
      compile_type dkenv env a
  | Term.App (cst, a, []) when is_sttfa_const sttfa_eta cst ->
      Ty (compile__type dkenv env a)
  | _ -> assert false

let rec compile_type_definition dkenv env (ty : Term.term) =
  match ty with
  | Term.Lam (_, x, _, ty) ->
      compile_type_definition dkenv (add_ty_var_dk env x) ty
  | _ ->
      let vars = env.ty in
      (vars, compile__type dkenv env ty)

(** {1 Terms} *)

open Result.Monad

type error = [ `CompileTermUnhandled of Term.term | `CompileTermUntypedLambda ]

let rec type_arity_of te =
  match te with ForallK (_, te) -> 1 + type_arity_of te | _ -> 0

let get_type_arity dkenv env lc name =
  try
    let ty = Api.Env.get_type dkenv lc name in
    type_arity_of (compile_wrapped_type dkenv env ty)
  with _ -> assert false

let rec compile__term dkenv env _te : (_te, [> error ]) result =
  match _te with
  | Term.DB (_, _, n) -> return (TeVar (term_var (get_dk_var env n)))
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
      let id = gen_fresh env id in
      let* _te' = compile__term dkenv (add_ty_var_dk env id) _te in
      return (AbsTy (type_var id, _te'))
  | Term.Lam (_, id, Some _ty, _te) ->
      let id = gen_fresh env id in
      let _ty' = compile_wrapped__type dkenv env _ty in
      let* _te' = compile__term dkenv (add_te_var_dk env id _ty') _te in
      return (Abs (term_var id, _ty', _te'))
  | Term.App (cst, _ty, [ Term.Lam (_, id, Some _, _te) ])
    when is_sttfa_const sttfa_forall cst ->
      let id = gen_fresh env id in
      let _ty' = compile__type dkenv env _ty in
      let* _te' = compile__term dkenv (add_te_var_dk env id _ty') _te in
      return (Forall (term_var id, _ty', _te'))
  | Term.App (cst, tel, [ ter ]) when is_sttfa_const sttfa_impl cst ->
      let* tel' = compile__term dkenv env tel in
      let* ter' = compile__term dkenv env ter in
      return (Impl (tel', ter'))
  | Term.App (Term.Const (lc, name), a, args) ->
      let cst' = of_name name in
      let i = get_type_arity dkenv env lc name in
      let args = a :: args in
      let ty_args, te_args = (take i args, drop i args) in
      let ty_args' = List.map (compile__type dkenv env) ty_args in
      let te_args' = List.map (compile__term dkenv env) te_args in
      let* te_args' =
        List.fold_right
          (fun te acc ->
            let* te = te in
            let* acc = acc in
            return (te :: acc))
          te_args' (return [])
      in
      return
      @@ List.fold_left
           (fun app arg -> App (app, arg))
           (Cst (cst', ty_args'))
           te_args'
  | Term.App (f, a, args) ->
      let* f' = compile__term dkenv env f in
      let args' = List.map (fun x -> compile__term dkenv env x) (a :: args) in
      let* args' =
        List.fold_right
          (fun te acc ->
            let* te = te in
            let* acc = acc in
            return (te :: acc))
          args' (return [])
      in
      return (List.fold_left (fun app arg -> App (app, arg)) f' args')
  | Term.Lam (_, _, None, _) -> Error `CompileTermUntypedLambda
  | Term.Const (_, cst) -> return (Cst (of_name cst, []))
  | x -> Error (`CompileTermUnhandled x)

let rec compile_term dkenv env te : (te, [> error ]) result =
  match te with
  | Term.App (cst, Term.Lam (_, x, Some _, te), [])
    when is_sttfa_const sttfa_forall_kind_prop cst ->
      let x = gen_fresh env x in
      let* te' = compile_term dkenv (add_ty_var_dk env x) te in
      return (ForallP (type_var x, te'))
  | _ ->
      let* te = compile__term dkenv env te in
      return (Te te)

let compile_wrapped_term dkenv env _te : (te, [> error ]) result =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile_term dkenv env te
  | x -> Error (`CompileTermUnhandled x)

let compile_wrapped__term dkenv env _te : (_te, [> error ]) result =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile__term dkenv env te
  | x -> Error (`CompileTermUnhandled x)
