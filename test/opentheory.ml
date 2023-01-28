let () =
  match Sttfaxport.(export OpenTheory "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
