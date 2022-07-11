open Sttfaxport

let () =
  match Systems.(export Lean "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
