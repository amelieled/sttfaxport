open Sttfaxport

let () =
  match Cmds.export Systems.Hollight "connectives.dk" with
  | Ok () -> ()
  | Error _ -> exit 1
