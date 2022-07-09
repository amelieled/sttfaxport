open Sttfaxport

let () =
  match Cmds.export Systems.OpenTheory "connectives.dk" with
  | Ok () -> ()
  | Error _ -> exit 1
