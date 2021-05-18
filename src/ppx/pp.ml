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

(* ------------------------------------------------ *)
(* ------------ Print type_declaration ------------ *)
(* ------------------------------------------------ *)

let list_to_str f l =
  let rec aux = function
    | [] -> ""
    | [ x ] -> f x
    | x :: xs -> f x ^ " ; " ^ aux xs
  in
  Printf.sprintf "[ %s ]" (aux l)

let rec type_decl_to_str td =
  Printf.sprintf
    {|
{
   ptype_name : %s;
   ptype_params : %s;
   ptype_cstrs : %s;
   ptype_kind : %s;
   ptype_private : %s;
   ptype_manifest : %s;
   ptype_attributes : %s;
   ptype_loc : %s
}
   |}
    td.ptype_name.txt
    (type_params_to_str td.ptype_params)
    (type_cstrs_to_str td.ptype_cstrs)
    (type_kind_to_str td.ptype_kind)
    (private_to_str td.ptype_private)
    (manifest_to_str td.ptype_manifest)
    (attributes_to_str td.ptype_attributes)
    "loc not printed"

and type_cstr_to_str (x, y, _) =
  Printf.sprintf "(%s, %s, _)" (core_type_to_str x) (core_type_to_str y)

and type_cstrs_to_str cstrs = list_to_str type_cstr_to_str cstrs

and type_param_to_str (ct, (_variance, _injectivity)) =
  Printf.sprintf "(%s, (_, _))" (core_type_to_str ct)

and type_params_to_str params = list_to_str type_param_to_str params

and core_type_to_str ct =
  match ct.ptyp_desc with
  (* TODO improve this printer *)
  | Ptyp_any -> "todo any"
  | Ptyp_var _string -> "todo var"
  | Ptyp_arrow (_arg_label, _ct1, _ct2) -> "todo arrow"
  | Ptyp_tuple _core_types -> "todo tuple"
  | Ptyp_constr (id, cts) ->
      Printf.sprintf
        "K (%s, %s)"
        (longident_to_str id.txt)
        (core_types_to_str cts)
  | _ -> "todo else core_type"

and core_types_to_str cts = list_to_str core_type_to_str cts

and type_kind_to_str = function
  | Ptype_abstract -> "abstract"
  (* TODO useful to print here *)
  | Ptype_variant _constrs -> "constructor_declaration list"
  | Ptype_record _labels -> "label_declaration list"
  | Ptype_open -> "open"

and attribute_to_str attr =
  Printf.sprintf "{attr_name : %s; _; _}" attr.attr_name.txt

and attributes_to_str attrs = list_to_str attribute_to_str attrs

and manifest_to_str ct = Option.fold ~none:"None" ~some:core_type_to_str ct

and longident_to_str = function
  | Lident s -> s
  | Ldot (lg, s) -> Printf.sprintf "%s.%s" (longident_to_str lg) s
  | Lapply (lg1, lg2) ->
      Printf.sprintf "%s %s" (longident_to_str lg1) (longident_to_str lg2)

and private_to_str = function Private -> "private" | Public -> "public"

let print_type_decl td = Printf.printf "%s\n" (type_decl_to_str td)
