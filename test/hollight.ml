let () =
  match Sttfaxport.(export Hollight "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
