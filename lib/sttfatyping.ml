open Ast
open Kernel
open Environ
module Dpp = Api.Pp.Default

(* TODO: Enhance error messages *)

let _infer dkenv env _te =
  let tedk = Decompile.decompile__term env.dk _te in
  let ty = Api.Env.infer dkenv ~ctx:env.dk tedk in
  Compile_type.compile__type dkenv env ty

let _eq env left right =
  let leftdk = Decompile.decompile__term env.dk left in
  let rightdk = Decompile.decompile__term env.dk right in
  Term.term_eq leftdk rightdk

let eq env left right =
  let leftdk = Decompile.decompile_term env.dk left in
  let rightdk = Decompile.decompile_term env.dk right in
  Term.term_eq leftdk rightdk

module ComputeStrategy = struct
  open Reduction

  let one_whnf = { default_cfg with nb_steps = Some 1; target = Whnf }
  let beta_snf = { default_cfg with select = Some (fun _ -> false) }
  let beta_steps n = { beta_snf with nb_steps = Some n }

  let delta (cst : Basic.name) =
    let open Rule in
    let filter = function Delta cst' -> Basic.name_eq cst' cst | _ -> false in
    {
      default_cfg with
      nb_steps = Some 1;
      beta = false;
      target = Snf;
      select = Some filter;
    }

  let beta_only = beta_snf
  let beta_one = { beta_only with nb_steps = Some 1; target = Whnf }

  let delta_only cst = delta cst
end

let _is_beta_normal dkenv env _te =
  let _tedk = Decompile.decompile__term env.dk _te in
  let _tedk' =
    Api.Env.unsafe_reduction dkenv ~red:ComputeStrategy.beta_snf _tedk
  in
  Term.term_eq _tedk _tedk'

let is_beta_normal dkenv env te =
  let tedk = Decompile.decompile_term env.dk te in
  let tedk' =
    Api.Env.unsafe_reduction dkenv ~red:ComputeStrategy.beta_snf tedk
  in
  Term.term_eq tedk tedk'

let subst dkenv env f a =
  let thm = (judgment_of f).thm in
  let te = Decompile.to_eps (Decompile.decompile_term env.dk thm) in
  let te = Api.Env.unsafe_reduction dkenv ~red:ComputeStrategy.one_whnf te in
  let _, b =
    match te with Term.Pi (_, _, tya, tyb) -> (tya, tyb) | _ -> assert false
  in
  let b' = Subst.subst b a in
  let b' = match b' with Term.App (_, a, []) -> a | _ -> assert false in
  let b' = Api.Env.unsafe_reduction dkenv ~red:ComputeStrategy.beta_one b' in
  Compile_type.compile_term dkenv env b'

