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

(** Module helping with OCaml types as QCheck.arbitrary *)

open Ppxlib

(** Convention generator name for any type name *)
val name : string -> string

(** Module representing OCaml primitives types supported *)
module Primitive : sig
  (** Converts string name of type into QCheck arbitrary *)
  val from_string : loc:location -> string -> expression
end

(** Converts a 'a type into a parametrizable QCheck arbitrary *)
val parametrizable_type : unit

(** Converts an applicated parametrizable type into a QCheck arbitrary 

    In this example, the type is an parametrizable list applicated to string:
    {[
    type t = string list [@@gen]

    let gen_t = QCheck.list QCheck.string
    ]}
*)
val constr_type :
  loc:location -> f:expression -> args:expression list -> unit -> expression

(** Transforms a longident into a QCheck.arbitrary 

    Multiples cases:
    - [X] the type is a identifier, either from a Primitive type or local scope
    - [X] the type comes from an outside module, we require a generator inside that
    outside module
    - [ ] the type is an application, can that type happens ? *)
val from_longident : loc:location -> longident -> expression

(** Transforms list of generators into a triple: 
    
    - expressions nested with QCheck.pair expression
    - generators names used in the expression
    - pattern according to the nested expression *)
val nest_gens :
  loc:location -> expression list -> expression * string list * pattern

(** Record auxiliar function, it extracts the pattern for a constructor application,
    the list of generators needed and the record expression
    
    type t = {left : int ; right = string }
    
    record' [int; string] [ left -> int ; right -> string ] =>
      - pattern : (x, y)
      - generators : pair QCheck.int QCheck.string
      - expression : { left = x ; right = string }

    This function has to be extracted from [record] because when trying to build
    a constructor using record, this expression needs the direct
    declaration of the record.
    
    Example
    {[
    type t = A of { something : int }
    
    let gen_t =
      QCheck.map (fun x -> A { something = x }) QCheck.int
    ]}

    In order to build such a QCheck.arbitrary we need the pattern, generators
    and record *)
val record' :
  loc:location ->
  gens:expression list ->
  label_declaration list ->
  pattern * expression * expression

(** Converts generators and labels declarations in a application of a record type

    Example:
    {[
    type t = { left : int ; right : string } [@@gen]

    let gen_t =
      QCheck.map (fun (x,y) -> { left = x ; right = y }) (QCheck.pair QCheck.int QCheck.string)
    ]}
 *)
val record :
  loc:location -> gens:expression list -> label_declaration list -> expression

(** Tuple auxiliar function, it extracts the pattern for a constructor application,
    the list of generators needed and the record expression
    
    type t = int * string
    
    record' [int; string] =>
      - pattern : (x, y)
      - generators : pair QCheck.int QCheck.string
      - expression : (int * string)

    This function has to be extracted from [tuple] because when trying to build
    a constructor using tuple, this expression needs the direct
    declaration of the record.
    
    Example
    {[
    type t = A of int * string
    
    let gen_t =
      QCheck.map (fun (x,y) -> A (int, string) ) QCheck.(pair int string)
    ]}

    In order to build such a QCheck.arbitrary we need the pattern, generators
    and record *)
val tuple' :
  loc:location -> expression list -> pattern * expression * expression

(** Converts generators in a application of a tuple
    
    Example:
    {[
    type t = int * int * int
    
    let gen_t =
      QCheck.map (fun (x,y,z) -> (x,y,z)) (QCheck.triple int int int)
    ]}
*)
val tuple : loc:location -> expression list -> expression

(** Converts a list of constructors into a single expression choosing the constructor

    Example:
    {[
    type t =
    | A
    | B
    | C
    [@@gen]

    let gen_t =
      let open QCheck in
      oneof [ make @@ Gen.return A ;
              make @@ Gen.return B ;
              make @@ Gen.return C ]
    ]}
*)
val constructors : loc:location -> expression list -> expression

(** Converts a constructor name into an expression constructor QCheck.arbitrary

    Example:
    {[
    type t =
    | A

    (* A => QCheck.make @@ QCheck.Gen.return A *)
    ]}

    An additional case is supported when constructor requires arguments
    
    Example:
    {[
    type t =
    | A of int * string
    | B of { left : int ; right : string }

    (* A => QCheck.map (fun (x,y) -> A (x,y)) QCheck.(pair int string)
       B => QCheck.map (fun (x,y) -> B { left = x ; right ; y }) QCheck.(pair int string) *)
    ]}
*)
val constructor :
  loc:location ->
  kname:string ->
  ?kargs:pattern * expression * expression ->
  unit ->
  expression

(** Create a QCheck.arbitrary using args name and body

    let name args = body *)
val gen :
  loc:location ->
  args:pattern list ->
  name:pattern ->
  body:expression ->
  unit ->
  structure_item
