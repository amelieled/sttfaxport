open Sttfaxport

let () =
  match Systems.(export Coq "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
