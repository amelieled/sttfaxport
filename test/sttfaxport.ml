open Sttfaxport

let ( let* ) = Result.bind

let () =
  let res =
    let* () = Cmds.export Systems.Coq [ "connectives.dk" ] in
    let* () = Cmds.export Systems.Pvs [ "connectives.dk" ] in
    Ok ()
  in
  match res with Ok () -> () | Error _ -> exit 1
