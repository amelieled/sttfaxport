open Ast
open Sttfadk
open Environ
module CType = Compile_type
module Dpp = Api.Pp.Default

type error = [ `CompiletermUnhandled of Term.term | `CompiletermUntypedLambda ]

let ( let* ) = Result.bind
let return = Result.ok

let rec type_arity_of te =
  match te with ForallK (_, te) -> 1 + type_arity_of te | _ -> 0

let get_type_arity dkenv env lc name =
  try
    let ty = Api.Env.get_type dkenv lc name in
    type_arity_of (CType.compile_wrapped_type dkenv env ty)
  with _ -> assert false

let rec compile__term dkenv env _te : (_te, [> error ]) result =
  match _te with
  | Term.DB (_, _, n) ->
      let var = get_dk_var env n in
      return (TeVar var)
  | Term.Lam (_, id, Some cst, _te) when is_sttfa_const sttfa_type cst ->
      let id = gen_fresh env [] id in
      let* _te' = compile__term dkenv (add_ty_var_dk env id) _te in
      return (AbsTy (soi id, _te'))
  | Term.Lam (_, id, Some _ty, _te) ->
      let id = gen_fresh env [] id in
      let _ty' = CType.compile_wrapped__type dkenv env _ty in
      let* _te' = compile__term dkenv (add_te_var_dk env id _ty') _te in
      return (Abs (soi id, _ty', _te'))
  | Term.App (cst, _ty, [ Term.Lam (_, id, Some _, _te) ])
    when is_sttfa_const sttfa_forall cst ->
      let id = gen_fresh env [] id in
      let _ty' = CType.compile__type dkenv env _ty in
      let* _te' = compile__term dkenv (add_te_var_dk env id _ty') _te in
      return (Forall (soi id, _ty', _te'))
  | Term.App (cst, tel, [ ter ]) when is_sttfa_const sttfa_impl cst ->
      let* tel' = compile__term dkenv env tel in
      let* ter' = compile__term dkenv env ter in
      return (Impl (tel', ter'))
  | Term.App (Term.Const (lc, name), a, args) ->
      let cst' = of_name name in
      let i = get_type_arity dkenv env lc name in
      let args = a :: args in
      let ty_args, te_args = (take i args, drop i args) in
      let ty_args' = List.map (CType.compile__type dkenv env) ty_args in
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
  | Term.Lam (_, _, None, _) -> Error `CompiletermUntypedLambda
  | Term.Const (_, cst) -> return (Cst (of_name cst, []))
  | x -> Error (`CompiletermUnhandled x)

let rec compile_term dkenv env te : (te, [> error ]) result =
  match te with
  | Term.App (cst, Term.Lam (_, x, Some _, te), [])
    when is_sttfa_const sttfa_forall_kind_prop cst ->
      let x = gen_fresh env [] x in
      let* te' = compile_term dkenv (add_ty_var_dk env x) te in
      return (ForallP (soi x, te'))
  | _ ->
      let* te = compile__term dkenv env te in
      return (Te te)

let compile_wrapped_term dkenv env _te : (te, [> error ]) result =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile_term dkenv env te
  | x -> Error (`CompiletermUnhandled x)

let compile_wrapped__term dkenv env _te : (_te, [> error ]) result =
  match _te with
  | Term.App (cst, te, []) when is_sttfa_const sttfa_eps cst ->
      compile__term dkenv env te
  | x -> Error (`CompiletermUnhandled x)
