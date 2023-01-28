let () =
  match Sttfaxport.(export Lean "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
