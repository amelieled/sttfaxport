open Sttfaxport

let () =
  match Systems.(export Hollight "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
