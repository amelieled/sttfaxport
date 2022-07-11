open Sttfaxport

let () =
  match Systems.(export OpenTheory "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
