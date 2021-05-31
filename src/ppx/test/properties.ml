(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Gens
open Ppxlib
module Error = Common.Error
module AH = Common.Ast_helpers
module E = AH.Expression
module P = AH.Pattern

type property_name = string

type arg = string

type gen = string

type property = property_name * arg list * gen list

type properties = property list

and t = properties

let check_help loc ppty xs ty =
  let (required, msg) =
    match ty with
    | `Args -> (Pbt.Properties.nb_of_gens ppty, "arguments")
    | `Gens -> (Pbt.Properties.nb_of_args ppty, "generators")
  in

  match required with
  | Some n ->
      let len = List.length xs in
      if n <> len then
        Error.property_gen_missing
          ~loc
          ~property:ppty
          ~required:n
          ~actual:len
          ()
  | None ->
      Printf.printf
        "Ppx_pbt (Warning): %s is your local property, can not check %s\n"
        ppty
        msg

let check_gens loc property_name gens = check_help loc property_name gens `Gens

let check_args loc property_name args = check_help loc property_name args `Args

let names_from_gens gens =
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

let pattern_from_gens loc gens =
  let rec create_pattern loc = function
    | Pair (x, y) ->
        [%pat? ([%p create_pattern loc x], [%p create_pattern loc y])]
    | Double (x, y) ->
        let arg_x = P.ppat_var ~loc x in
        let arg_y = P.ppat_var ~loc y in
        Ppat_tuple [ arg_x; arg_y ] |> P.pattern ~loc
    | Simple x -> P.ppat_var ~loc x
  in
  let args = names_from_gens gens in
  (create_pattern loc args, args)

let args_to_expr loc args =
  let f x = (Nolabel, E.pexp_lident ~loc x) in
  List.map f args

let call_property loc fun_name (name, args, gens) =
  let args =
    fun_name :: args @ Gens.nested_pairs_to_list gens |> args_to_expr loc
  in
  let f = Pbt.Properties.from_string ~loc name in
  E.pexp_apply ~loc ~f ~args ()
