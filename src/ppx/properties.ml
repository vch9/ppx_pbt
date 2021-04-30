open Gens
open Ppxlib

type property_name = string

type arg = string

type property = property_name * arg list

type properties = property list

and t = properties

(* TODO: change to formatted version *)
let rec properties_to_str = function
  | [ p ] -> Format.sprintf "%s\n" (property_to_str p)
  | p1 :: p2 :: pps ->
      Format.sprintf
        "%s ;\n%s"
        (property_to_str p1)
        (properties_to_str (p2 :: pps))
  | [] -> assert false

and property_to_str (name, args) =
  Format.sprintf "%s[%s]" name (args_to_str args)

and args_to_str = function
  | [] -> ""
  | [ arg ] -> arg
  | arg1 :: arg2 :: args ->
      Format.sprintf "%s ; %s" arg1 (args_to_str (arg2 :: args))

let properties_gens = [ ("commutative", 2) ]

(* TODO: if it returns None, should we create a Pbt.Properties.x ? *)
let builtin_properties loc x =
  [ ("commutative", [%expr Pbt.Properties.commutative]) ] |> List.assoc_opt x

let rec takes_n list n =
  match (list, n) with
  | (_, 0) -> []
  | (x :: xs, n) -> x :: takes_n xs (n - 1)
  (* Not enough elements in the list:
     not enough generators for the property
     TODO: generate human readable error *)
  | _ -> failwith "TODO ERROR"

let get_gens property_name args =
  match List.assoc_opt property_name properties_gens with
  | Some n -> takes_n args n
  (* A design choice must be choose here,
     - reject unknown property ?
     - inline it as a call to a function ? *)
  | None -> failwith "TODO ERROR"

let create_assoc_args x =
  let id = ref 0 in
  let create_fresh_name i =
    let x = !i in
    i := !i + 1 ;
    "gen_" ^ string_of_int x
  in
  let rec aux = function
    | Pair (x, y) ->
        let x = aux x in
        let y = aux y in
        Pair (x, y)
    | Double _ ->
        let x = create_fresh_name id in
        let y = create_fresh_name id in
        Double (x, y)
    | Simple _ -> Simple (create_fresh_name id)
  in
  aux x

let build_pat loc pat =
  { ppat_desc = pat; ppat_loc = loc; ppat_loc_stack = []; ppat_attributes = [] }

let build_var loc var = Ppat_var { txt = var; loc } |> build_pat loc

let pattern_from_gens loc gens =
  let rec create_pattern loc = function
    | Pair (x, y) ->
        [%pat? ([%p create_pattern loc x], [%p create_pattern loc y])]
    | Double (x, y) ->
        let arg_x = build_var loc x in
        let arg_y = build_var loc y in
        Ppat_tuple [ arg_x; arg_y ] |> build_pat loc
    | Simple x ->
        let arg_x = build_var loc x in
        Ppat_tuple [ arg_x ] |> build_pat loc
  in
  let args = create_assoc_args gens in
  (create_pattern loc args, args)

let args_to_expr loc args =
  let f x = (Nolabel, Helpers.build_ident loc x) in
  List.map f args

let call_property loc fun_name name args =
  let args = fun_name :: Gens.nested_pairs_to_list args |> args_to_expr loc in
  match builtin_properties loc name with
  | Some fun_expr -> Helpers.build_apply loc fun_expr args
  | None -> failwith "TODO"
