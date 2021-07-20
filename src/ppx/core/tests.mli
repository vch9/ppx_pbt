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

(** Module handling the test generation.

    Entry point is {!properties_to_test}. *)

open Ppxlib

(** [properties_to_tests loc name sig_item properties] create a test for every
    property inside [properties].

    Returns the new list of tests as a structure_item list including
    QCheck.Test.t and add them to the test suite. *)
val properties_to_test :
  loc:location ->
  name:string ->
  ?sig_item:signature_item ->
  Properties.t ->
  structure_item list

(** [property_to_test loc name properties] create a test for a single property
    on the function called [name].
    Returns the test and its identifier as a string.

    The optional paramater [?sig_item] can be used to infer required generator
    using {{:https://github.com/vch9/ppx_deriving_qcheck}ppx_deriving_qcheck}. *)
val property_to_test :
  loc:location ->
  name:string ->
  ?sig_item:signature_item ->
  Properties.property ->
  structure_item * string

(** Extract generators from generators identifiers, also check if the number
    of generator is correct only if the property is known to our program *)
val gens_to_test :
  loc:location ->
  signature_item option ->
  Properties.property_name ->
  Properties.gen list ->
  expression * expression Common.Helpers.Pairs.nested_pairs

(** Create the boolean function used in QCheck tests *)
val pbt_to_test :
  loc:location ->
  string ->
  Properties.property_name ->
  expression Common.Helpers.Pairs.nested_pairs ->
  Properties.arg list ->
  expression
