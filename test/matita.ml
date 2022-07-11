open Sttfaxport

let () =
  match Systems.(export Matita "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
