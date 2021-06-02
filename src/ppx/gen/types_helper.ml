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
module Error = Common.Error
module E = Common.Ast_helpers.Expression
module P = Common.Ast_helpers.Pattern
module H = Common.Helpers
module Pairs = Common.Helpers.Pairs

let name s = Printf.sprintf "gen_%s" s

module Primitive = struct
  let from_string ~loc ?rec_types = function
    | "int" -> [%expr QCheck.int]
    | "string" -> [%expr QCheck.string]
    | "char" -> [%expr QCheck.char]
    | "bool" -> [%expr QCheck.bool]
    | "float" -> [%expr QCheck.float]
    | "unit" -> [%expr QCheck.unit]
    | "option" -> [%expr QCheck.option]
    | "list" -> [%expr QCheck.list]
    | s -> (
        let gen = E.pexp_lident ~loc @@ name s in
        match rec_types with
        | None -> gen
        | Some xs ->
            if List.mem s xs then
              E.pexp_apply
                ~loc
                ~f:(E.pexp_lident ~loc @@ name (s ^ "'"))
                ~args:[ (Nolabel, [%expr n - 1]) ]
                ()
            else gen)
end

let constr_type ~loc ~f ~args () =
  let args = List.map (fun x -> (Nolabel, x)) args in
  E.pexp_apply ~loc ~f ~args ()

let from_longident ~loc ?rec_types = function
  | Lident s -> Primitive.from_string ~loc ?rec_types s
  | Ldot (lg, s) -> E.pexp_ident ~loc @@ H.mk_loc ~loc @@ Ldot (lg, name s)
  | _ ->
      Error.case_unsupported
        ~loc
        ~case:"Ppx.Gen.Types.create_gen_from_longident"
        ()

let nest_gens ~loc gens =
  let open Pairs in
  let gens = nest_generators gens in
  let (pat, gens_name) = pattern_from_gens loc gens in
  let gens = nested_pairs_to_expr loc gens in
  let gens_name = nested_pairs_to_list gens_name in

  (gens, gens_name, pat)

let record' ~loc ~gens xs =
  let (gens_expr, gens_name, gens_pat) = nest_gens ~loc gens in
  let fields =
    List.map2
      (fun x gen ->
        let name = x.pld_name.txt in
        (H.mk_loc ~loc (Lident name), E.pexp_lident ~loc gen))
      xs
      gens_name
  in
  let body = E.pexp_record ~loc ~fields None in
  (gens_pat, gens_expr, body)

let record ~loc ~gens xs =
  let (gens_pat, gens_expr, body) = record' ~loc ~gens xs in
  [%expr QCheck.map (fun [%p gens_pat] -> [%e body]) [%e gens_expr]]

let tuple' ~loc gens =
  let (gens_expr, gens_name, gens_pat) = nest_gens ~loc gens in
  let body = List.map E.(pexp_lident ~loc) gens_name |> E.pexp_tuple ~loc in
  (gens_pat, gens_expr, body)

let tuple ~loc gens =
  let (gens_pat, gens_expr, body) = tuple' ~loc gens in
  [%expr QCheck.map (fun [%p gens_pat] -> [%e body]) [%e gens_expr]]

let constructors ~loc xs =
  let xs = E.pexp_list ~loc xs in
  [%expr QCheck.oneof [%e xs]]

let constructor ~loc ~kname ?kargs () =
  let kname = H.mk_loc ~loc @@ Lident kname in

  match kargs with
  | None ->
      let kname = E.pexp_construct ~loc ~kname ~kargs:None () in
      [%expr QCheck.make @@ QCheck.Gen.return [%e kname]]
  | Some (pat, gens, expr) ->
      let expr = E.pexp_construct ~loc ~kname ~kargs:(Some expr) () in
      [%expr QCheck.map (fun [%p pat] -> [%e expr]) [%e gens]]

let tree ~loc ~ty ~leaves ~nodes () =
  let _ = ty in

  let leaves = E.pexp_list ~loc leaves and nodes = E.pexp_list ~loc nodes in

  let rec_gen =
    [%expr
      function 0 -> QCheck.oneof [%e leaves] | n -> QCheck.oneof [%e nodes]]
  in
  rec_gen

let rec curry_args ~loc args body =
  match args with
  | [] -> body
  | x :: xs -> [%expr fun [%p x] -> [%e curry_args ~loc xs body]]

let gen ~loc ~flag ~args ~name ~body () =
  let body = curry_args ~loc args body in
  let pat_name = P.ppat_var ~loc name in
  let name' = name ^ "'" in
  let pat_name' = P.ppat_var ~loc name' in
  if not flag then [%stri let [%p pat_name] = [%e body]]
  else
    let f = E.pexp_lident ~loc name' in
    let args = [ (Nolabel, [%expr 5]) ] in
    let pat_expr = E.pexp_apply ~loc ~f ~args () in
    [%stri
      let [%p pat_name] =
        let rec [%p pat_name'] = [%e body] in
        [%e pat_expr]]
