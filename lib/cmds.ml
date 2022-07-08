open Extras

type ('a, 'b) register_eq =
  'a Api.Processor.t * 'b Api.Processor.t ->
  ('a Api.Processor.t, 'b Api.Processor.t) Api.Processor.Registration.equal
  option

module SttfaCompile = struct
  (* NOTE the compiler works for one module only. *)
  type t = Ast.ast

  let items = ref []

  let handle_entry env entry =
    let modu = Api.Env.get_name env in
    items :=
      ( Compile.compile_entry env entry,
        Deps.dep_of_entry [ Sttfadk.sttfa_module; modu ] entry )
      :: !items

  let get_data env =
    let modu = Api.Env.get_name env in
    let items = List.rev !items in
    let dep = List.fold_left StrSet.union StrSet.empty (List.map snd items) in
    Ast.
      {
        md = Kernel.Basic.string_of_mident modu;
        dep;
        items = List.map fst items;
      }
end

type _ Api.Processor.t += SttfaCompile : Ast.ast Api.Processor.t

let equal_sttfacompile (type a b) : (a, b) register_eq = function
  | SttfaCompile, SttfaCompile ->
      Some (Api.Processor.Registration.Refl SttfaCompile)
  | _ -> None

let () =
  Api.Processor.Registration.register_processor SttfaCompile
    { equal = equal_sttfacompile }
    (module SttfaCompile)

let export ?(path = []) ?(oc = stdout) sys file =
  List.iter Api.Files.add_path path;
  let ast = Api.Processor.handle_files [ file ] SttfaCompile in
  let (module Exporter) = Systems.exporter sys in
  Exporter.print_ast oc ast;
  Ok ()
