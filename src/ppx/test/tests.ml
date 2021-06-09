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

(*------ Parse properties ------*)
let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Parser.properties Lexer.token lexbuf_pps

open Ppxlib
module Error = Common.Error
module AH = Common.Ast_helpers
module E = AH.Expression
module P = AH.Pattern
module Info = Common.Helpers.Info
module Payload = Common.Payload
module Pairs = Common.Helpers.Pairs

(* Replace Properties.t into list of structure item *)
let rec properties_to_test ~loc ~name properties =
  let (tests, names) =
    List.split @@ List.map (property_to_test ~loc ~name) properties
  in
  let names = E.pexp_list ~loc (List.map (E.pexp_lident ~loc) names) in
  let add_runner = [%stri let () = Runner.add_tests [%e names]] in
  tests @ [ add_runner ]

and property_to_test ~loc ~name (property, args, gens) =
  let (pat_name, expr_name, test_name) = name_to_test ~loc name property in
  let (gens_expr, gens) = gens_to_test ~loc property gens in
  let tested_fun = pbt_to_test ~loc name property gens args in

  let test =
    [%stri
      let [%p pat_name] =
        QCheck.Test.make ~name:[%e expr_name] [%e gens_expr] [%e tested_fun]]
  in
  (test, test_name)

and gens_to_test ~loc property gens =
  let _ = Properties.check_gens loc property gens in
  let gens = Gens.replace_gens loc gens in
  let gens = Pairs.nest_generators gens in
  let expr_gens = Pairs.nested_pairs_to_expr loc gens in
  (expr_gens, gens)

and name_to_test ~loc name property =
  let qcheck_name = Format.sprintf "%s_is_%s" name property in
  let test_name = Format.sprintf "test_%s" qcheck_name in
  let expr_name = E.pexp_string ~loc qcheck_name in
  let pat_name = P.ppat_var ~loc test_name in

  (pat_name, expr_name, test_name)

and pbt_to_test ~loc name property gens args =
  let open Properties in
  let _ = check_args loc property args in
  let (fun_pattern, gens) =
    Pairs.pattern_from_gens loc (fun x -> "gen_" ^ x) gens
  in
  let call = Properties.call_property loc name (property, args, gens) in
  [%expr fun [%p fun_pattern] -> [%e call]]

let replace_pbt (xs : Info.t list) =
  match xs with
  (* Structure item by construction can attach only one property *)
  | [ info ] ->
      let loc = Info.get_loc info in
      let name = Info.get_name info in
      Payload.pbt_from_attribute (Option.get (Info.get_attribute info))
      |> from_string
      |> properties_to_test ~loc ~name
  (* TODO: better error management *)
  | _ -> assert false
