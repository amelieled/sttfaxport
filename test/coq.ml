open Sttfaxport

let () =
  match Cmds.export Systems.Coq "connectives.dk" with
  | Ok () -> ()
  | Error _ -> exit 1
