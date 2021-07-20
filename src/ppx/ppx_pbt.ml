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

let print_argv () = Array.fold_left (fun acc x -> acc ^ " " ^ x) "" Sys.argv

let ignore () =
  Array.exists
    (fun x ->
      Filename.check_suffix x ".expected.ml"
      || Filename.check_suffix x ".pp.ml"
      || Filename.check_suffix x ".pp.mli")
    Sys.argv

(** [get_file_name_sig sigi] returns the file name where [sigi] is located *)
let get_file_name_sig sigi =
  sigi.psig_loc.loc_start.pos_fname |> Filename.remove_extension

(** [get_file_name_str stri] returns the file name where [stri] is located *)
let get_file_name_str stri =
  stri.pstr_loc.loc_start.pos_fname |> Filename.remove_extension

class mapper =
  object (_self)
    inherit Ast_traverse.map as super

    method! structure str =
      if not (ignore ()) then
        let file_name = get_file_name_str @@ List.hd str in
        if Interface.interface file_name then Interface.inline_impl_tests str
        else super#structure str
      else super#structure str

    method! signature sigs =
      if not (ignore ()) then
        let file_name = get_file_name_sig @@ List.hd sigs in
        Interface.intf file_name sigs
      else super#signature sigs

    method! structure_item stri =
      if not (ignore ()) then failwith "TODO stri"
      else super#structure_item stri
    (* let expand stri =
     *   let loc = stri.pstr_loc in
     * 
     *   let infos_pbt = get_attributes stri |> filter_attributes pbt_name in
     *   let n_pbt = List.length infos_pbt in
     * 
     *   match stri with
     *   (\* let f args = expr [@@pbt <properties>] *\)
     *   | [%stri let [%p? f] = [%e? _body]] when n_pbt > 0 ->
     *       let infos =
     *         let name = extract_name_from_pat f in
     *         List.map (Helpers.Info.update_name name) infos_pbt
     *       in
     *       AH.Structure.str_include ~loc (stri :: Test.Tests.replace_pbt infos)
     *   (\* default cases *\)
     *   | x -> super#structure_item x
     * in
     * 
     * if not !ignore then expand stri else super#structure_item stri *)
  end

(* let impl xs =l
 *   let file_name = get_file_name_str @@ List.hd xs in
 * 
 *   if not (ignore ()) then
 *     let () = Env.fetch_env file_name in
 *     if file_name = Env.get_file_name () then inline_impl_tests xs else xs
 *   else xs *)

let () =
  let mapper = new mapper in
  Driver.register_transformation
    "ppx_test"
    ~intf:mapper#signature
    ~impl:mapper#structure
