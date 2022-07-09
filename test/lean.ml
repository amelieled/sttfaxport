open Sttfaxport

let () =
  match Cmds.export Systems.Lean "connectives.dk" with
  | Ok () -> ()
  | Error _ -> exit 1
