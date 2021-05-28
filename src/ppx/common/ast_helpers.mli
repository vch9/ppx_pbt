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

(** Ast_helpers with 4 sub_modules:
    - Expressions
    - Pattern
    - Structure *)

open Ppxlib

(* Helper module with builders for Ppxlib.Ast.expression *)
module Expression : sig
  val expression :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    expression_desc ->
    expression

  (** Build a Pexp_let *)
  val pexp_let :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    ?flag:rec_flag ->
    ?bindings:value_binding list ->
    expression ->
    expression

  (** Build a Pexp_constant with Pconst_string *)
  val pexp_string :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    string ->
    expression

  (** Build a list of expression *)
  val pexp_list : ?loc:location -> expression list -> expression

  (** Build a Pexp_apply *)
  val pexp_apply :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    f:expression ->
    args:(arg_label * expression) list ->
    unit ->
    expression

  (** Build a Pexp_apply *)
  val pexp_tuple :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    expression list ->
    expression

  (** Build a Pexp_apply *)
  val pexp_construct :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    kname:longident loc ->
    kargs:expression option ->
    unit ->
    expression

  (** Build a Pexp_ident *)
  val pexp_ident :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    longident_loc ->
    expression

  (** Build a Pexp_ident with a Lident *)
  val pexp_lident :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    string ->
    expression
end

(* Helper module with builders for Ppxlib.Ast.pattern *)
module Pattern : sig
  val pattern :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    pattern_desc ->
    pattern

  (** Build a Ppat_var *)
  val ppat_var :
    ?loc:location ->
    ?loc_stack:location_stack ->
    ?attributes:attributes ->
    string ->
    pattern

  (** Build a Ppat_any *)
  val ppat_any : ?loc:location -> unit -> pattern
end

(* Helper module with builders for Ppxlib.Ast.Structure *)
module Structure : sig
  val value_binding :
    ?loc:location ->
    ?pat:pattern ->
    ?attributes:attributes ->
    expression ->
    value_binding

  val structure_item : ?loc:location -> structure_item_desc -> structure_item

  val pexp_include : ?loc:location -> structure_item list -> structure_item
end
