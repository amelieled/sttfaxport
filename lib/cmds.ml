type printer = out_channel -> unit

module CoqExport : Api.Processor.S with type t = printer list = struct
  type t = printer list

  let printers = ref []

  let handle_entry env entry =
    let item = Compile.compile_entry env entry in
    let stritem = Coq.string_of_item env item in
    printers := (fun oc -> Printf.fprintf oc "%s" stritem) :: !printers

  let get_data _ = List.rev !printers
end

type _ Api.Processor.t += CoqExport : printer list Api.Processor.t

let equal_coqexport (type a b) :
    a Api.Processor.t * b Api.Processor.t ->
    (a Api.Processor.t, b Api.Processor.t) Api.Processor.Registration.equal
    option = function
  | CoqExport, CoqExport -> Some (Api.Processor.Registration.Refl CoqExport)
  | _ -> None

let () =
  Api.Processor.Registration.register_processor CoqExport { equal = equal_coqexport }
    (module CoqExport)

let export ?(path=[]) ?(oc=stdout) sys files =
  List.iter Api.Files.add_path path;
  match sys with
  | Systems.Coq ->
      let printers = Api.Processor.handle_files files CoqExport in
      List.iter (fun e -> e oc; Printf.fprintf oc "\n") printers;
      Ok ()
  | _ -> assert false
