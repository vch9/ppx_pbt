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
module Env = Local_env

let print_argv () = Array.fold_left (fun acc x -> acc ^ " " ^ x) "" Sys.argv

let ignore () =
  Array.exists
    (fun x ->
      Filename.check_suffix x ".expected.ml"
      || Filename.check_suffix x ".pp.ml"
      || Filename.check_suffix x ".pp.mli")
    Sys.argv

let pbt_name = "pbt"

let filter_attributes expected xs =
  List.filter (fun attr -> attr.attr_name.txt = expected) xs

let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Test.Parser.properties Test.Lexer.token lexbuf_pps

(** [extract_name_from_pattern pat] tries to extract the function name
    located in the pattern {[ let <pattern> = <expr> ]} *)
let extract_name_from_pattern pat : string option =
  match pat.ppat_desc with
  | Ppat_any -> None
  | Ppat_var { txt = x; _ } -> Some x
  | _ ->
      let x = Obj.Extension_constructor.of_val pat in
      let x = Obj.Extension_constructor.name x in
      Printf.printf "extract_name_from_pattern: %s\n" x ;
      None

(** [get_file_name_sig sigi] returns the file name where [sigi] is located *)
let get_file_name_sig sigi =
  sigi.psig_loc.loc_start.pos_fname |> Filename.remove_extension

(** [get_file_name_str stri] returns the file name where [stri] is located *)
let get_file_name_str stri =
  stri.pstr_loc.loc_start.pos_fname |> Filename.remove_extension

(** [get_properties attributes] returns the list propertiy inside [attributes]

    Step 1: keep every attribute named {!pbt_name}
    Step 3: extract each attribute's payload, which must be a string constant
    Step 3: parse the properties
    Step 4: concat every properties into a single list

    Implicitly the function returns an empty list of properties if there is not
    properties attached on the attributes *)
let get_properties attributes =
  filter_attributes pbt_name attributes
  |> List.map Common.Payload.pbt_from_attribute
  |> List.map from_string |> List.concat

(** [find_attributes code_path sig] does an in-depth course of a signature_item.

    It looks for [Psig_value] where there's an attribute "pbt" attached to it.
    Every occurences of signature item with the "pbt" attribute is stored in the
    local environment, in order to be use in {!inline_impl_tests}

    Once a recursive case is reached, we store the current path and go deeper. *)
let rec find_attributes x = find_attributes_signature_item [] x

and find_attributes_signature_item code_path sigi =
  match sigi.psig_desc with
  | Psig_value vd -> (
      let properties = get_properties vd.pval_attributes in
      match properties with
      | [] -> ()
      | _ ->
          let path = List.rev code_path in
          let name = vd.pval_name.txt in
          let value = sigi in
          Env.add_env ~path ~properties ~value name)
  | Psig_module { pmd_name = { txt = name; _ }; pmd_type = mtd; _ } ->
      (* Can the signature_item can have None as a name ? *)
      let name = Option.get name in
      let code_path = `Psig_module name :: code_path in
      find_attributes_module_type code_path mtd
  | _ -> ()

and find_attributes_module_type code_path mtd =
  match mtd.pmty_desc with
  | Pmty_signature sigs ->
      List.iter (find_attributes_signature_item code_path) sigs
  | _ -> failwith "TODO"

(** [find_and_replace does the actual in-depth course of structured_item according
    to the path found in a signature_item *)
let find_and_replace (path, properties, name, sigi) stri : structure_item =
  let loc = stri.pstr_loc in

  match (path, stri) with
  | ([], [%stri let [%p? pat] = [%e? _]])
  | ([], [%stri let rec [%p? pat] = [%e? _]]) -> (
      match extract_name_from_pattern pat with
      | Some name' when name = name' ->
          let tests =
            Test.Tests.properties_to_test ~name ?sig_item:sigi properties
          in
          Common.Ast_helpers.Structure.str_include ~loc @@ stri :: tests
      | _ -> stri)
  | (_, _) -> stri

(** [inline_impl_tests env str] replaces the according specification in mli with the
    actual implementation.

    For each Psig_value found in {!check_attributes} stored in the environment, we follow the
    path between the recursives structure_items until we eventually find the according
    Pstr_value using {!find_and_inline} *)
let inline_impl_tests structure : structure_item list =
  List.fold_left
    (fun structure psig_value ->
      let path = Env.get_path psig_value in
      let properties = Env.get_properties psig_value in
      let name = Env.get_name psig_value in
      let sig_item = Env.get_value psig_value in
      List.map (find_and_replace (path, properties, name, sig_item)) structure)
    structure
    (Env.get_psig_values ())

let intf xs =
  (if not (ignore ()) then
   let file_name = get_file_name_sig @@ List.hd xs in
   let () = Env.init_env ~file_name () in
   let () = List.iter find_attributes xs in
   let () = Env.store_env () in
   ()) ;
  xs

let impl xs =
  let file_name = get_file_name_str @@ List.hd xs in

  if not (ignore ()) then
    let () = Env.fetch_env file_name in
    if file_name = Env.get_file_name () then inline_impl_tests xs else xs
  else xs

let () = Driver.register_transformation "ppx_test" ~intf ~impl
