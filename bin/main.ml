let () =
  let file = Sys.argv.(1) in
  match Sttfaxport.Systems.(export Pvs file) with
  | Ok () -> ()
  | Error () ->
      Printf.eprintf "oops";
      exit 1
