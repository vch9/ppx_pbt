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

(** Modules handling generators *)
open Ppxlib

(** Takes generators with their names and replace them with expression
    using [Pbt.Properties.from_string] *)
val replace_gens : location -> string list -> expression list

type 'a nested_pairs =
  | Pair of 'a nested_pairs * 'a nested_pairs
  | Double of 'a * 'a
  | Simple of 'a

(** Takes a list of expression and nest them into pairs, in order
    to be used with [QCheck.pair] combinator 

    example:
   nest_generators [a] => a
   nest_generators [a;b] => Double a b
   nest_generators [a;b;c] => Pair (Simple a) (Double b c)
   nest_generators [a;b;c;d] => Pair (Double a b) (Double c d) *)
val nest_generators : expression list -> expression nested_pairs

(** Transforms nested pairs of expressions into an expression using
    [QCheck.pair combinator] *)
val nested_pairs_to_expr : location -> expression nested_pairs -> expression

(** Transforms nested pairs of 'a into a list of 'a, roundtrip with [nest_generators] *)
val nested_pairs_to_list : 'a nested_pairs -> 'a list
