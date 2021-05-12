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
open Ppxlib
open Error

let rec create_gen_from_td td =
  (* TODO use attribute location *)
  let loc = !Ast_helper.default_loc in
  let name =
    (* TODO export gen_%s in a function *)
    Helpers.build_pattern_var loc @@ Printf.sprintf "gen_%s" td.ptype_name.txt
  in
  let body =
    Option.fold (* TODO what happens when the manifest is None ? *)
      ~none:[%expr "TODO"]
      ~some:create_gen_from_core_type
      td.ptype_manifest
  in
  [%stri let [%p name] = [%e body]]

and create_gen_from_core_type ct =
  match ct.ptyp_desc with
  | Ptyp_constr (lg, _c_types) ->
      (* TODO what is c_types ? *)
      create_gen_from_longident lg.txt
  | Ptyp_tuple elems -> create_gen_from_tuple elems
  | _ -> raise (CaseUnsupported "create_gen_from_core_type")

and create_gen_from_longident = function
  | Lident s -> (
      (* TODO use attribute location *)
      let loc = !Ast_helper.default_loc in
      match Gens.builtin_generators loc s with
      | Some e -> e
      (* TODO export gen_%s in a function *)
      | None -> Helpers.build_ident loc @@ "gen_" ^ s)
  | _ -> raise (CaseUnsupported "create_gen_from_longident")

and create_gen_from_tuple elems =
  let loc = !Ast_helper.default_loc in
  let gens = List.map create_gen_from_core_type elems in

  match gens with
  | [ g1; g2 ] -> [%expr QCheck.pair [%e g1] [%e g2]]
  | [ g1; g2; g3 ] -> [%expr QCheck.triple [%e g1] [%e g2] [%e g3]]
  | [ g1; g2; g3; g4 ] -> [%expr QCheck.quad [%e g1] [%e g2] [%e g3] [%e g4]]
  | _ -> raise (CaseUnsupported "Tuple contains more than 4 elements")

let replace_stri stri =
  let gen =
    match stri.pstr_desc with
    (* TODO multiple types declaration ? *)
    | Pstr_type (_, [ x ]) ->
        (* TODO remove this print *)
        print_type_decl x ;
        Some (create_gen_from_td x)
    (* TODO structure item inside structure item *)
    | _ -> None
  in
  Option.fold ~none:[ stri ] ~some:(fun x -> [ stri; x ]) gen

let rec stri_contains_gen stri =
  match stri.pstr_desc with
  | Pstr_type (_, [ x ]) -> type_decl_contains_gen x
  | _ -> false

and type_decl_contains_gen td =
  List.exists attribute_contains_gen td.ptype_attributes

and attribute_contains_gen attr =
  (* TODO do not hardcore gen here *)
  attr.attr_name.txt = "gen"
