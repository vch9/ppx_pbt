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

(** Module representing the properties as payload *)

open Ppxlib
open Common.Helpers.Pairs

(** Type representing properties *)

type property_name = string

type arg = string

type gen = string

type property = property_name * arg list * gen list

type properties = property list

and t = properties

(** Checks gens check if the number of given generator is equals to the expected
    number of generators.
    Raise an exception if expected and actual are different *)
val check_gens : location -> string -> 'a list -> unit

(** Checks args check if the number of given arguments is equals to the expected
    number of arguments.
    Raise an exception if expected and actual are different *)
val check_args : location -> string -> 'a list -> unit

(** Associate a unique name to the nested generators *)
val names_from_gens : 'a nested_pairs -> string nested_pairs

(** Create a pattern from the nested generators names,
    it also returns the generators from [names_from_gens] in order
    to be used afterward *)
val pattern_from_gens :
  location -> 'a nested_pairs -> pattern * string nested_pairs

(** Create an expression from the property *)
val call_property :
  location -> string -> string * string list * string nested_pairs -> expression
