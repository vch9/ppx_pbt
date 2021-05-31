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

(** Module entry point, expand structure_item with tests *)
open Ppxlib

(** Parse a string into a Properties.t, using Menhir *)
val from_string : string -> Properties.t

(** Create tests using [property_to_test] on every property *)
val properties_to_test :
  loc:location -> name:string -> Properties.t -> structure_item list

(** Create the test based on the property, also returns the name of the test
    in order to be added to the test suite *)
val property_to_test :
  loc:location -> name:string -> Properties.property -> structure_item * string

(** Extract generators from generators identifiers, also check if the number
    of generator is correct only if the property is known to our program *)
val gens_to_test :
  loc:location ->
  Properties.property_name ->
  Properties.gen list ->
  expression * expression Gens.nested_pairs

(** Create names for a test

    (pattern, expression, string)

    They are later used to create QCheck tests

    pattern    -> pattern representing the test name
    expression -> expression representing the test structure item
    string     -> name of the test as string for error messages *)
val name_to_test :
  loc:location ->
  string ->
  Properties.property_name ->
  pattern * expression * string

(** Create the boolean function used in QCheck tests *)
val pbt_to_test :
  loc:location ->
  string ->
  Properties.property_name ->
  expression Gens.nested_pairs ->
  Properties.arg list ->
  expression

(** Module entry point, returns tests generated from the infos *)
val replace_pbt : Common.Helpers.Info.t list -> structure_item list
