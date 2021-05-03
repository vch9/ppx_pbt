open Gens
open Ppxlib
open Error

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

(* Builtin properties contains two fields:
   expr <= Ast.expression calling the Pbt.Properties.f
   n_gens <= Number of generators required to call Pbt.Properties f *)
type builtin_properties = { expr : expression; n_gens : int }

let builtin_properties x =
  let loc = !Ast_helper.default_loc in
  [
    ("commutative", { expr = [%expr Pbt.Properties.commutative]; n_gens = 2 });
    ("associative", { expr = [%expr Pbt.Properties.associative]; n_gens = 3 });
  ]
  |> List.assoc_opt x

let rec takes_n list n =
  match (list, n) with
  | (_, 0) -> []
  | (x :: xs, n) -> x :: takes_n xs (n - 1)
  | _ -> assert false
(* The function is called only if there's enough arguments *)

(* We get generators from the payload arguments

   [@@pbt {| property_name[arg0, arg1, .., argN] |} ]

   To each property_name is attached a number of required generators,
   see builtin_properties

   If the property needs n generators, n are taken from the list of arguments.
   Otherwise, an exception is raised *)
let get_gens property_name args =
  match builtin_properties property_name with
  | Some { n_gens = n; _ } ->
      let len = List.length args in
      if n > len then raise (PropertyGeneratorsMissing (property_name, n, len))
      else takes_n args n
  | None -> raise (PropertyNotSupported property_name)

(* Applied arguments depends on the given generators

   example:

   (fun gen0 -> <property> <tested_fun> gen0)
   (fun (gen0, gen1) -> <property> <tested_fun> gen0 gen1)
   (fun (gen0, (gen1, gen2)) -> <property> <tested_fun> gen0 gen1 gen2) *)
let create_assoc_args gens =
  let id = ref 0 in
  let create_fresh_name i =
    let x = !i in
    i := !i + 1 ;
    "gen_" ^ string_of_int x
  in
  (* Replace_by_id replace the generators pattern by identifiers refering to the
     function pattern *)
  let rec replace_by_id = function
    | Pair (x, y) ->
        let x = replace_by_id x in
        let y = replace_by_id y in
        Pair (x, y)
    | Double _ ->
        let x = create_fresh_name id in
        let y = create_fresh_name id in
        Double (x, y)
    | Simple _ -> Simple (create_fresh_name id)
  in
  replace_by_id gens

(* Transform the nested_pairs from Generators into Pattern.tuple *)
let pattern_from_gens loc gens =
  let rec create_pattern loc = function
    | Pair (x, y) ->
        [%pat? ([%p create_pattern loc x], [%p create_pattern loc y])]
    | Double (x, y) ->
        let arg_x = Helpers.build_pattern_var loc x in
        let arg_y = Helpers.build_pattern_var loc y in
        Ppat_tuple [ arg_x; arg_y ] |> Helpers.build_pattern loc
    | Simple x ->
        let arg_x = Helpers.build_pattern_var loc x in
        Ppat_tuple [ arg_x ] |> Helpers.build_pattern loc
  in
  let args = create_assoc_args gens in
  (* Pattern is returned with the assoc between generators and the identifier
     given in create_assoc_args *)
  (create_pattern loc args, args)

let args_to_expr loc args =
  let f x = (Nolabel, Helpers.build_ident loc x) in
  List.map f args

(* Build the call to the property intented to be tested

   (fun .. -> Pbt.Property.property_name gen0 gen1 ..) *)
let call_property loc fun_name name args =
  let args = fun_name :: Gens.nested_pairs_to_list args |> args_to_expr loc in
  match builtin_properties name with
  | Some { expr = fun_expr; _ } -> Helpers.build_apply loc fun_expr args
  | None -> raise (PropertyNotSupported name)
