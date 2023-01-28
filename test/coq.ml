let () =
  match Sttfaxport.(export Coq "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
