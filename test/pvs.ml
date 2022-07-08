open Sttfaxport

let () =
  match Cmds.export Systems.Pvs "connectives.dk" with
  | Ok () -> ()
  | Error _ -> exit 1
