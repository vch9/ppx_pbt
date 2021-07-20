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

let filter_attributes expected xs =
  List.filter (fun attr -> attr.attr_name.txt = expected) xs

let pbt_name = "pbt"

let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Core.Parser.properties Core.Lexer.token lexbuf_pps
