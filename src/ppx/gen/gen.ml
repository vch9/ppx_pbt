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
module Info = Common.Helpers.Info
module Error = Common.Error
module T = Types_helper
module P = Common.Ast_helpers.Pattern
module PP = Common.Pp

let extract_args ~loc params =
  let to_pat (ct, _) =
    match ct.ptyp_desc with
    | Ptyp_var s -> P.ppat_var ~loc @@ "gen_" ^ s
    | _ -> Error.case_unsupported ~loc ~case:"Ppx.Gen.gen.extract_args" ()
  in
  List.map to_pat params

let rec is_recursive ~loc ~ty = function
  | Ptype_variant cstrs ->
      List.exists (is_recursive_constructor_decl ~loc ~ty) cstrs
  | Ptype_record xs -> is_recursive_label_declarations ~loc ~ty xs
  | _ -> Error.case_unsupported ~case:"Ppx.Gen.is_recursive" ()

and is_recursive_constructor_decl ~loc ~ty cd =
  match cd.pcd_args with
  | Pcstr_tuple cts -> List.exists (is_recursive_core_type ~loc ~ty) cts
  | Pcstr_record xs -> is_recursive_label_declarations ~loc ~ty xs

and is_recursive_label_declarations ~loc ~ty xs =
  let labels =
    List.filter_map
      (fun x ->
        let loc = x.pld_type.ptyp_loc in
        match x.pld_type.ptyp_desc with
        | Ptyp_var s -> if s = ty then Some loc else None
        | Ptyp_constr (lg, _) ->
            let s = PP.longident_to_str lg.txt in
            if s = ty then Some loc else None
        | _ -> None)
      xs
  in
  match labels with
  | [] -> false
  | _ ->
      Error.location_error
        ~loc
        ~msg:"ppx_pbt does not supports recursive record"
        ()

and is_recursive_core_type ~loc ~ty ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = lg; _ }, cts) ->
      PP.longident_to_str lg = ty
      || List.exists (is_recursive_core_type ~loc ~ty) cts
  | _ -> false

let rec from_core_type ~loc ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = ty; _ }, []) -> T.from_longident ~loc ty
  | Ptyp_constr ({ txt = ty; _ }, args) ->
      let f = T.from_longident ~loc ty in
      let args = List.map (from_core_type ~loc) args in
      T.constr_type ~loc ~f ~args ()
  | Ptyp_tuple xs -> from_tuple ~loc xs
  | Ptyp_var s -> T.Primitive.from_string ~loc s
  | _ -> Error.case_unsupported ~loc ~case:"Ppx.Gen.Types.from_core_type" ()

and from_type_kind ~loc ~ty = function
  | Ptype_record xs -> from_record ~loc xs
  | Ptype_variant xs -> from_variant ~loc ~ty xs
  | _ -> failwith "TODO 1"

and from_record ~loc label_decls =
  let gens = List.map (fun x -> from_core_type ~loc x.pld_type) label_decls in
  T.record ~loc ~gens label_decls

and from_tuple ~loc cts =
  let gens = List.map (from_core_type ~loc) cts in
  T.tuple ~loc gens

and from_variant ~loc ~ty xs =
  if is_recursive ~loc ~ty @@ Ptype_variant xs then
    Error.case_unsupported ~case:"constructor declarations recursive" ()
  else
    let gens = List.map (from_constructor_decl ~loc) xs in
    T.constructors ~loc gens

and from_constructor_decl ~loc x =
  let kname = x.pcd_name.txt in
  let f ~kargs = T.constructor ~loc ~kname ~kargs () in
  match x.pcd_args with
  | Pcstr_tuple [] | Pcstr_record [] -> T.constructor ~loc ~kname ()
  | Pcstr_tuple xs ->
      let gens = List.map (from_core_type ~loc) xs in
      let kargs = T.tuple' ~loc gens in
      f ~kargs
  | Pcstr_record xs ->
      let gens = List.map (fun x -> from_core_type ~loc x.pld_type) xs in
      let kargs = T.record' ~loc ~gens xs in
      f ~kargs

let from_type_declaration ~loc td =
  let ty = td.ptype_name.txt in
  let name = P.ppat_var ~loc @@ T.name ty in

  let body =
    match td.ptype_manifest with
    | None -> from_type_kind ~loc ~ty td.ptype_kind
    | Some ct -> from_core_type ~loc ct
  in

  let args = extract_args ~loc td.ptype_params in

  T.gen ~loc ~args ~name ~body ()

let replace_stri infos stri =
  let info = List.hd infos in
  match stri.pstr_desc with
  | Pstr_type (_, [ x ]) ->
      let loc = Info.get_loc info in
      from_type_declaration ~loc x
  | _ -> Error.case_unsupported ~case:"Ppx.Gen.replace_stri" ()
