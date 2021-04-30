open Ppxlib

(* Exhaustive list of builtin generators
   If the function returns None -> the generator must be provided by the user *)
let builtin_generators loc x =
  [ ("int", [%expr Pbt.Gens.int]) ] |> List.assoc_opt x

(* Either replace gen_ids by builtin generators or with identifiers to
   locally provided generators

   example:

   [@@pbt {| commutative[int, my_gen_int] |}]
   =>
   QCheck.pair (Pbt.Properties.int, my_gen_int)

   int belongs to builtin generators and is rewrited as the reference to the
   generator in Pbt.Properties module, whereas my_gen_int needs to be available
   at current scope *)
let replace_gens loc gen_ids =
  let replace gen_id =
    match builtin_generators loc gen_id with
    | Some gen -> gen
    | None -> Helpers.build_ident loc gen_id
  in
  List.map replace gen_ids

let split_even_list list =
  let n = List.length list in
  let middle = n / 2 in
  let i = ref (-1) in
  List.partition
    (fun _ ->
      i := !i + 1 ;
      !i < middle)
    list

type 'a nested_pairs =
  | Pair of 'a nested_pairs * 'a nested_pairs
  | Double of 'a * 'a
  | Simple of 'a

(* [int, int] -> (pair int int) *)
let rec nest_generators gens =
  match List.length gens with
  | 0 -> failwith "TODO" (* insert unit ? *)
  | 1 -> Simple (List.hd gens)
  | 2 -> ( match gens with [ x; y ] -> Double (x, y) | _ -> assert false)
  | n when n mod 2 = 0 ->
      let (l1, l2) = split_even_list gens in
      Pair (nest_generators l1, nest_generators l2)
  | _ -> Pair (Simple (List.hd gens), nest_generators (List.tl gens))

let rec nested_pairs_to_expr loc = function
  | Simple expr -> expr
  | Pair (x, y) ->
      [%expr
        QCheck.pair
          [%e nested_pairs_to_expr loc x]
          [%e nested_pairs_to_expr loc y]]
  | Double (x, y) -> [%expr QCheck.pair [%e x] [%e y]]

let rec nested_pairs_to_list = function
  | Simple x -> [ x ]
  | Double (x, y) -> [ x; y ]
  | Pair (x, y) ->
      let left = nested_pairs_to_list x in
      let right = nested_pairs_to_list y in
      left @ right
