type printer = out_channel -> unit

module type EXP = sig
  val string_of_item : Api.Env.t -> Ast.item -> string
end

let processor_of_exporter (module Exp : EXP) :
    (module Api.Processor.S with type t = printer list) =
  (module struct
    let printers = ref []

    type t = printer list

    let handle_entry env entry =
      let item = Compile.compile_entry env entry in
      let stritem = Exp.string_of_item env item in
      printers := (fun oc -> Printf.fprintf oc "%s" stritem) :: !printers

    let get_data _ = List.rev !printers
  end : Api.Processor.S
    with type t = printer list)

module CoqExport = (val processor_of_exporter (module Coq))

type _ Api.Processor.t += CoqExport : printer list Api.Processor.t

let equal_coqexport (type a b) :
    a Api.Processor.t * b Api.Processor.t ->
    (a Api.Processor.t, b Api.Processor.t) Api.Processor.Registration.equal
    option = function
  | CoqExport, CoqExport -> Some (Api.Processor.Registration.Refl CoqExport)
  | _ -> None

module PvsExport = (val processor_of_exporter (module Pvs))

type _ Api.Processor.t += PvsExport : printer list Api.Processor.t

let equal_pvsexport (type a b) :
    a Api.Processor.t * b Api.Processor.t ->
    (a Api.Processor.t, b Api.Processor.t) Api.Processor.Registration.equal
    option = function
  | PvsExport, PvsExport -> Some (Api.Processor.Registration.Refl PvsExport)
  | _ -> None

let () =
  Api.Processor.Registration.register_processor CoqExport
    { equal = equal_coqexport }
    (module CoqExport);
  Api.Processor.Registration.register_processor PvsExport
    { equal = equal_pvsexport }
    (module PvsExport)

let export ?(path = []) ?(oc = stdout) sys files =
  List.iter Api.Files.add_path path;
  match sys with
  | Systems.Coq ->
      let printers = Api.Processor.handle_files files CoqExport in
      List.iter
        (fun e ->
          e oc;
          Printf.fprintf oc "\n")
        printers;
      Ok ()
  | Systems.Pvs ->
      let printers = Api.Processor.handle_files files PvsExport in
      List.iter
        (fun e ->
          e oc;
          Printf.fprintf oc "\n")
        printers;
      Ok ()
  | _ -> assert false
