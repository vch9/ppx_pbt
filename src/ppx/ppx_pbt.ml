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

(*------ Find attributes in Ast.structure_item ------*)

let pbt_name = "pbt"

let gen_name = "gen"

let ignore = ref false

let filter_attributes expected xs =
  let open Helpers in
  List.filter (fun attr -> attr.attr_name.txt = expected) xs
  |> List.map (fun attr ->
         create_info ~payload:attr.attr_payload ~loc:attr.attr_loc ())

let get_attributes stri =
  match stri.pstr_desc with
  | Pstr_eval (_, attributes) -> attributes
  | Pstr_value (_, vbs) ->
      List.map (fun vb -> vb.pvb_attributes) vbs |> List.concat
  | _ -> []

let extract_name_from_pat pat =
  match pat.ppat_desc with
  | Ppat_var { txt = name; _ } -> name
  | _ -> raise (CaseUnsupported "Function name can not be extracted")

class mapper =
  object (_self)
    inherit Ast_traverse.map as super

    method! structure_item stri =
      let expand stri =
        let loc = stri.pstr_loc in
        let infos = get_attributes stri |> filter_attributes pbt_name in
        let n = List.length infos in

        match stri with
        (* let f args = expr [@@pbt <properties>] *)
        | [%stri let [%p? f] = [%e? _body]] when n > 0 ->
            let infos =
              let name = extract_name_from_pat f in
              List.map (Helpers.update_name name) infos
            in
            Helpers.build_include loc (stri :: Tests.replace_pbt infos)
        (* default cases *)
        | x -> super#structure_item x
      in

      try if not !ignore then expand stri else super#structure_item stri
      with e ->
        Error.print_exception e ;
        raise InternalError
  end

let () =
  let mapper = new mapper in
  Driver.register_transformation "ppx_pbt" ~impl:mapper#structure
