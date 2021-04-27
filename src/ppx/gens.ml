open Ppxlib

let builtin_generators loc = [ ("int", [%expr Qbc.Gens.int]) ]

(* [int, int] -> (pair int int) *)
let nest_generators gens =
  let _n = List.length gens in
  failwith "TODO"

let replace_gens gen_ids =
  let replace gen_id =
    match List.assoc_opt builtin_generators gen_id with
    | Some _gen -> failwith "TODO"
    | None -> failwith "TODO"
  in
  List.map replace gen_ids
