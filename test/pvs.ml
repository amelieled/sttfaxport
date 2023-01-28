let () =
  match Sttfaxport.(export Pvs "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
