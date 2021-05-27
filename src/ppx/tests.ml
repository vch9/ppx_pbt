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
open Error
open Helpers

let get_tested_fun_pattern pattern =
  match pattern.ppat_desc with
  | Ppat_var { txt = str; _ } -> Some str
  | _ -> None

(* Extract fun name we want to test *)
let rec get_tested_fun_values_binding values_bindings =
  (* A structured item should contains only one pbt attribute *)
  List.map get_tested_fun_value_binding values_bindings |> List.hd

and get_tested_fun_expression_desc expr_desc =
  match expr_desc with
  | Pexp_let (_, values_binding, _) ->
      get_tested_fun_values_binding values_binding
  | Pexp_ident longident_loc -> get_tested_fun_longident_loc longident_loc
  | _ -> raise (CaseUnsupported "get_tested_fun_expression_desc")

and get_tested_fun_value_binding value_binding =
  match get_tested_fun_pattern value_binding.pvb_pat with
  (* In case of let f <pattern> = <expr>, the function name is located inside
     the value_binding.pattern *)
  | Some str -> str
  (* Otherwise, we look for the function name in the expression *)
  | None -> get_tested_fun_expression_desc value_binding.pvb_expr.pexp_desc

and get_tested_fun_longident_loc longident_loc =
  match longident_loc.txt with
  | Ldot (_, str) -> str
  | _ -> raise (CaseUnsupported "get_tested_fun_longident_loc")

(* Replace Properties.t into list of structure item *)
let rec properties_to_test ~loc ~name properties =
  let (tests, names) =
    List.split @@ List.map (property_to_test ~loc ~name) properties
  in
  let names =
    Helpers.build_list loc (List.map (Helpers.build_lident loc) names)
  in
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
  let open Gens in
  let _ = Properties.check_gens loc property gens in
  let gens = replace_gens loc gens in
  let gens = nest_generators gens in
  let expr_gens = nested_pairs_to_expr loc gens in
  (expr_gens, gens)

and name_to_test ~loc name property =
  let qcheck_name = Format.sprintf "%s_is_%s" name property in
  let test_name = Format.sprintf "test_%s" qcheck_name in
  let expr_name = Helpers.build_string loc qcheck_name in
  let pat_name = Helpers.build_pattern_var loc test_name in

  (pat_name, expr_name, test_name)

and pbt_to_test ~loc name property gens args =
  let open Properties in
  let _ = check_args loc property args in
  let (fun_pattern, gens) = Properties.pattern_from_gens loc gens in
  let call = Properties.call_property loc name (property, args, gens) in
  [%expr fun [%p fun_pattern] -> [%e call]]

let replace_pbt = function
  (* Structure item by construction can attach only one property *)
  | [ info ] ->
      let loc = info.stri_loc in
      let name = info.stri_name in
      Payload.extract_pbt_from_payload (Option.get info.stri_payload)
      |> from_string
      |> properties_to_test ~loc ~name
  (* TODO: better error management *)
  | _ -> assert false
