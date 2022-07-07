let () =
  match Sttfaxport.Cmds.export Sttfaxport.Systems.Coq ["connectives.dk"] with
  | Ok _ -> ()
  | Error _ -> exit 1
