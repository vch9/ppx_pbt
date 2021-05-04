let ignore () =
  Array.exists (fun x -> Filename.check_suffix x ".expected") Sys.argv

let () =
  if ignore () then Qbc_ppx.Ppx_qbc.ignore := true ;
  Ppxlib.Driver.standalone ()
