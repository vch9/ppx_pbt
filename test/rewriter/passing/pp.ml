let ignore () =
  Array.exists (fun x -> Filename.check_suffix x ".expected.ml") Sys.argv

let () =
  if ignore () then Qbc_ppx.Ppx_qbc.ignore := true ;
  Ppxlib.Driver.standalone ()
