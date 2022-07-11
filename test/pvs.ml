open Sttfaxport

let () =
  match Systems.(export Pvs "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
