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

(* Helper function to build tuple

   returns (expression * name list * pattern)

   example:
   build_nested_gens __loc__ [int, int, string] =>

     QCheck.Pair
        Pbt.Gens.int
        (QCheck.Pair
            Pbt.Gens.int
            Pbt.Gens.int),
     [gen_0, gen_1, gen_2],
     (gen_0, (gen_1, gen_2))
*)

let build_nested_gens loc gens =
  let gens = Gens.nest_generators gens in
  let (pat, gens_name) = Properties.pattern_from_gens loc gens in
  let gens = Gens.nested_pairs_to_expr loc gens in
  let gens_name = Gens.nested_pairs_to_list gens_name in

  (gens, gens_name, pat)

(* Helper function to check if the type is recursive *)
let rec is_recursive type_name = function
  | Ptype_variant cstrs ->
      List.exists (is_recursive_constructor_decl type_name) cstrs
  | _ -> raise (CaseUnsupported "is_recursive")

and is_recursive_constructor_decl type_name cd =
  match cd.pcd_args with
  | Pcstr_tuple cts -> List.exists (is_recursive_core_type type_name) cts
  | Pcstr_record _ -> raise (CaseUnsupported "is_recursive_contructor_decl")

and is_recursive_core_type type_name ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = lg; _ }, _) -> Pp.longident_to_str lg = type_name
  | _ -> false

(* ------------------------------------------------ *)
(* ------- Create gen from type declaration ------- *)
(* ------------------------------------------------ *)

let rec create_gen_from_td td =
  (* TODO use attribute location *)
  let loc = !Ast_helper.default_loc in
  let type_name = td.ptype_name.txt in
  let name =
    (* TODO export gen_%s in a function *)
    Helpers.build_pattern_var loc @@ Printf.sprintf "gen_%s" type_name
  in
  let body =
    match td.ptype_manifest with
    | None -> create_gen_from_kind type_name td.ptype_kind
    | Some ct -> create_gen_from_core_type ct
  in
  [%stri let [%p name] = [%e body]]

and create_gen_from_core_type ct =
  match ct.ptyp_desc with
  | Ptyp_constr (lg, _c_types) ->
      (* TODO what is c_types ? *)
      create_gen_from_longident lg.txt
  | Ptyp_tuple elems -> create_gen_from_tuple elems
  | _ -> raise (CaseUnsupported "create_gen_from_core_type")

and create_gen_from_kind type_name kind =
  match kind with
  | Ptype_variant cstrs_decl ->
      if is_recursive type_name kind then create_from_kind_rec kind
      else
        (* TODO loc here *)
        let loc = !Ast_helper.default_loc in
        let gens =
          List.map create_gen_from_constructor_decl cstrs_decl
          |> Helpers.build_list loc
        in
        [%expr QCheck.oneof [%e gens]]
  | _ -> raise (CaseUnsupported "create_gen_from_kind")

and create_from_kind_rec _kind =
  (* TODO *)
  raise (CaseUnsupported "create_from_kind_rec")

and create_gen_from_constructor_decl cd =
  (* TODO loc here *)
  let loc = !Ast_helper.default_loc in

  let (gens, gens_name, pat) =
    build_nested_gens loc @@ create_gen_from_cstr_args cd.pcd_args
  in

  let k_name = Helpers.build_longident loc @@ Lident cd.pcd_name.txt in

  let k_args =
    List.map (Helpers.build_ident loc) gens_name |> Helpers.build_tuple loc
  in

  let build = Helpers.build_construct loc k_name k_args in

  [%expr QCheck.map (fun [%p pat] -> [%e build]) [%e gens]]

and create_gen_from_cstr_args = function
  | Pcstr_tuple cts -> List.map create_gen_from_core_type cts
  | _ -> raise (CaseUnsupported "create_gen_from_cstr_arg")

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
  | [ g1 ] -> g1
  | [ g1; g2 ] -> [%expr QCheck.pair [%e g1] [%e g2]]
  | [ g1; g2; g3 ] -> [%expr QCheck.triple [%e g1] [%e g2] [%e g3]]
  | [ g1; g2; g3; g4 ] -> [%expr QCheck.quad [%e g1] [%e g2] [%e g3] [%e g4]]
  | gens ->
      let (gens, gens_name, pat) = build_nested_gens loc gens in
      let build =
        List.map (Helpers.build_ident loc) gens_name |> Helpers.build_tuple loc
      in
      [%expr QCheck.map (fun [%p pat] -> [%e build]) [%e gens]]

let replace_stri stri =
  let gen =
    match stri.pstr_desc with
    (* TODO multiple types declaration ? *)
    | Pstr_type (_, [ x ]) -> Some (create_gen_from_td x)
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
