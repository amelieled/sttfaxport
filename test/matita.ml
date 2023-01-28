let () =
  match Sttfaxport.(export Matita "connectives.dk") with
  | Ok () -> ()
  | Error _ -> exit 1
