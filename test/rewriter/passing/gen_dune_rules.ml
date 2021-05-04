let output_stanzas filename =
  let base = Filename.remove_extension filename in
  Printf.printf
    {|
(library
 (name %s)
 (modules %s)
 (libraries qbc_ppx))

(rule
 (targets %s.expected.format)
 (deps (:pp pp.exe) (:input %s.expected))
 (action (bash "./%%{pp} --impl %%{input} -o %%{targets}")))

(rule
 (targets %s.actual)
 (deps (:pp pp.exe) (:input %s.ml))
 (action (bash "./%%{pp} --impl %%{input} -o %%{targets}")))

(rule
 (alias runtest)
 (deps (:actual %s.actual) (:expected %s.expected.format))
 (action (diff %%{expected} %%{actual})))
|}
    base
    base
    base
    base
    base
    base
    base
    base

let is_error_test = function
  | "pp.ml" -> false
  | "gen_dune_rules.ml" -> false
  | filename -> Filename.check_suffix filename ".ml"

let () =
  Sys.readdir "." |> Array.to_list |> List.sort String.compare
  |> List.filter is_error_test |> List.iter output_stanzas
