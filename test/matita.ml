open Sttfaxport

let () =
  match Cmds.export Systems.Matita "connectives.dk" with
  | Ok () -> ()
  | Error _ -> exit 1
