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
module H = Common.Helpers
module AH = Common.Ast_helpers
module Gens = Test.Gens
module Properties = Test.Properties
module Pp = Common.Pp

(* Helper function to build tuple

   returns (expression * name list * pattern)

   example:
   build_nested_gens __loc__ [int, int, string] =>

     QCheck.Pair
        Pbt.Gens.int
        (QCheck.Pair
            Pbt.Gens.int
            Pbt.Gens.int),
     [gen_0, gen_1, gen_2],
     (gen_0, (gen_1, gen_2))
*)

let build_nested_gens loc gens =
  let gens = Gens.nest_generators gens in
  let (pat, gens_name) = Properties.pattern_from_gens loc gens in
  let gens = Gens.nested_pairs_to_expr loc gens in
  let gens_name = Gens.nested_pairs_to_list gens_name in

  (gens, gens_name, pat)

(* Returns length of Konstructor arguments

   type t =
   | Node of int * t * t
   | Leaf

   len_cstr_args (Node of int * t * t) => 3
   len_cstr_args (Leaf)                => 0 *)
let len_cstr_args = function
  | Pcstr_tuple xs -> List.length xs
  | Pcstr_record xs -> List.length xs

(* Helper function to check if the type is recursive *)
let rec is_recursive type_name = function
  | Ptype_variant cstrs ->
      List.exists (is_recursive_constructor_decl type_name) cstrs
  | Ptype_record xs -> is_recursive_label_declarations type_name xs
  | _ -> Error.case_unsupported ~case:"Ppx.Gen.is_recursive" ()

and is_recursive_constructor_decl type_name cd =
  match cd.pcd_args with
  | Pcstr_tuple cts -> List.exists (is_recursive_core_type type_name) cts
  | Pcstr_record xs -> is_recursive_label_declarations type_name xs

and is_recursive_label_declarations type_name xs =
  let labels =
    List.filter_map
      (fun x ->
        let loc = x.pld_type.ptyp_loc in
        match x.pld_type.ptyp_desc with
        | Ptyp_var s -> if s = type_name then Some loc else None
        | Ptyp_constr (lg, _) ->
            let s = Pp.longident_to_str lg.txt in
            if s = type_name then Some loc else None
        | _ -> None)
      xs
  in
  match labels with
  | [] -> false
  | loc :: _ ->
      Error.location_error
        ~loc
        ~msg:"ppx_pbt does not supports recursive record"
        ()

and is_recursive_core_type type_name ct =
  match ct.ptyp_desc with
  | Ptyp_constr ({ txt = lg; _ }, cts) ->
      Pp.longident_to_str lg = type_name
      || List.exists (is_recursive_core_type type_name) cts
  | _ -> false

(* ------------------------------------------------ *)
(* ------- Create gen from type declaration ------- *)
(* ------------------------------------------------ *)

let gen_name s = "gen_" ^ s

(*
   Curryfication of arguments

   curry_args __loc__ [x; y; z] body

   ->

   fun x -> fun y -> fun z -> [body]
*)
let rec curry_args ~loc args body =
  match args with
  | [] -> body
  | x :: xs -> [%expr fun [%p x] -> [%e curry_args ~loc xs body]]

let extract_args ~loc params =
  let to_pat (ct, _) =
    match ct.ptyp_desc with
    | Ptyp_var s -> AH.Pattern.ppat_var ~loc @@ gen_name s
    | _ -> Error.case_unsupported ~case:"Ppx.Gen.extract_args" ()
  in

  List.map to_pat params

let rec create_gen_from_td ~loc td =
  let type_name = td.ptype_name.txt in
  let name = AH.Pattern.ppat_var ~loc @@ gen_name type_name in
  let args = extract_args ~loc td.ptype_params in
  let body =
    match td.ptype_manifest with
    (* TODO, can everything be ignored when manifest is present ? *)
    | None -> create_gen_from_kind ~loc type_name td.ptype_kind
    | Some ct -> create_gen_from_core_type ~loc ct
  in
  let body = curry_args ~loc args body in
  [%stri let [%p name] = [%e body]]

and create_gen_from_core_type ~loc ct =
  match ct.ptyp_desc with
  (*
    Primitives types are found in Ptyp_constr when c_types is empty

    Example with c_types = []:

    {[
    type t1 = int [@@gen]
    type t2 = string [@@gen]
    type t3 = t1 [@@gen]

    let gen_t1 = QCheck.int
    let gen_t2 = QCheck.string
    let gen_t3 = gen_t1
    ]}

    Example with c_types <> []:

    {[
    type 'a t1 = 'a list [@@gen]
    type 'a t2 = 'a option [@@gen]

    let gen_t1 gen_a = QCheck.list gen_a
    let gen_t2 gen_a = QCheck.option gen_a
    ]}
   *)
  | Ptyp_constr (lg, c_types) -> (
      match c_types with
      | [] -> create_gen_from_longident ~loc lg.txt
      | xs ->
          let gen_t = create_gen_from_longident ~loc lg.txt in
          let gen_args =
            List.map (fun x -> (Nolabel, create_gen_from_core_type ~loc x)) xs
          in
          AH.Expression.pexp_apply ~loc ~f:gen_t ~args:gen_args ())
  (*
      Tuples are translated in different ways based on their sizes:

      Example :

      {[
      type two = int * int [@@gen]
      type three = int * int * int [@@gen]
      type four = int * int * int * int [@@gen]
      type five = int * int * int * int * int [@@gen]

      let gen_two = QCheck.(pair int int)
      let gen_three = QCheck.(triple int int int)
      let gen_four = QCheck.(triple int int int int)
      let gen_five = QCheck.(pair (pair int (pair int int) (pair int int))
      ]}
  *)
  | Ptyp_tuple elems -> create_gen_from_tuple ~loc elems
  | Ptyp_var s -> create_gen_from_string ~loc s
  | _ ->
      Error.case_unsupported ~loc ~case:"Ppx.Gen.create_gen_from_core_type" ()

and create_gen_from_kind ~loc type_name kind =
  match kind with
  | Ptype_variant cstrs_decl ->
      if is_recursive type_name kind then create_from_kind_rec kind
      else
        let gens =
          List.map (create_gen_from_constructor_decl ~loc) cstrs_decl
          |> AH.Expression.pexp_list ~loc
        in
        [%expr QCheck.oneof [%e gens]]
  | Ptype_record label_decls ->
      let _ = is_recursive type_name kind in
      (* If is_recursive returns true, the programs raises an exception *)
      let (pat, gens, body) = create_record ~loc label_decls in
      [%expr QCheck.map (fun [%p pat] -> [%e body]) [%e gens]]
  | _ -> Error.case_unsupported ~loc ~case:"Ppx.Gen.create_gen_from_kind" ()

(* Helper function to build a record

   returns (pat, record, gens) *)
and create_record ~loc label_decls =
  let gens =
    List.map (fun x -> create_gen_from_core_type ~loc x.pld_type) label_decls
  in
  let (gens, gens_name, pat) = build_nested_gens loc gens in

  let body_record =
    List.map2
      (fun x gen ->
        let name = x.pld_name.txt in
        (H.mk_loc ~loc (Lident name), AH.Expression.pexp_lident ~loc gen))
      label_decls
      gens_name
  in
  let body = AH.Expression.expression ~loc @@ Pexp_record (body_record, None) in

  (pat, gens, body)

and create_from_kind_rec _kind =
  (* TODO *)
  Error.case_unsupported ~case:"Ppx.Gen.create_from_kind_rec" ()

(*
  type constructor_declaration = {
       pcd_name : string loc;
       pcd_args : constructor_arguments;
       pcd_res : core_type option;
       pcd_loc : location;
       pcd_attributes : attributes;
  }

  pcd_name       <- name of the constructor (A, K, Node, Leaf, Some ..)
  pcd_args       <- arguments of the constructor (Some of int, Node of int * tree * tree ..)
  pcd_res        <- TODO: what is pcd_res ?
  pcd_attributes <- TODO: put weight on a constructor declaration

  Example without pcd_attributes:

  {[
  type num =
  | Int of int
  | Float of float
  [@@gen]

  let gen_num =
    QCheck.oneof [
      QCheck.map (fun x -> Int x) QCheck.int ;
      QCheck.map (fun y -> Float y) QCheck.float ;
    ]
  ]}

  Example with pcd_attributes:

  {[
  type num =
  | Int of int [@gen 2]
  | Float of float [@gen 1]
  [@@gen]

  let gen_num =
    QCheck.frequency [
      (2, QCheck.map (fun x -> Int x) QCheck.int) ;
      (1, QCheck.map (fun y -> Float y) QCheck.float) ;
    ]
  ]}

  Example of pcd_args:

  {[
  type t = A | B | C
  [@@gen]

  let gen_t =
    QCheck.oneof [
      QCheck.make @@ QCheck.Gen.return A ;
      QCheck.make @@ QCheck.Gen.return B ;
      QCheck.make @@ QCheck.Gen.return C ;
    ]
  ]}

  [
    pcd_name = A ; pcd_args = [];
    pcd_name = B ; pcd_args = [];
    pcd_name = C ; pcd_args = [];
  ]

  {[
  type t = Simple of int
  | Double of int * int
  | Triple of int * int * int
  [@@gen]

  let gen_t =
    QCheck.oneof [
      QCheck.map (fun x -> Simple x) QCheck.int ;
      QCheck.map (fun (x,y) -> Double (x,y)) (QCheck.pair QCheck.int QCheck.int) ;
      QCheck.map (fun (x,y,z) -> Triple (x,y,z) (QCheck.triple QCheck.int QCheck.int QCheck.int) ;
    ]
  ]}

  [
    pcd_name = Simple ; pcd_args = [int];
    pcd_name = Double ; pcd_args = [int; int];
    pcd_name = Triple ; pcd_args = [int; int; int];
  ]
*)
and create_gen_from_constructor_decl ~loc cd =
  if len_cstr_args cd.pcd_args > 0 then create_gen_from_cd_with_args ~loc cd
  else create_gen_from_cd_without_args ~loc cd

and create_gen_from_cd_without_args ~loc cd =
  let kname = H.mk_loc ~loc (Lident cd.pcd_name.txt) in
  let k = AH.Expression.pexp_construct ~loc ~kname ~kargs:None () in

  [%expr QCheck.make @@ QCheck.Gen.return [%e k]]

and create_gen_from_cd_with_args ~loc cd =
  let kname = H.mk_loc ~loc @@ Lident cd.pcd_name.txt in

  match cd.pcd_args with
  | Pcstr_tuple cts ->
      let (gens, gens_name, pat) =
        build_nested_gens loc @@ List.map (create_gen_from_core_type ~loc) cts
      in

      let k_args =
        List.map (AH.Expression.pexp_lident ~loc) gens_name
        |> AH.Expression.pexp_tuple ~loc
      in

      let build =
        AH.Expression.pexp_construct ~loc ~kname ~kargs:(Some k_args) ()
      in

      [%expr QCheck.map (fun [%p pat] -> [%e build]) [%e gens]]
  | Pcstr_record xs ->
      let (pat, gens, record) = create_record ~loc xs in
      let build =
        AH.Expression.pexp_construct ~loc ~kname ~kargs:(Some record) ()
      in

      [%expr QCheck.map (fun [%p pat] -> [%e build]) [%e gens]]

and create_gen_from_string ~loc s =
  (*
  match Gens.builtin_generators loc s with
  | Some e -> e
  | None -> Helpers.build_lident loc @@ gen_name s *)
  let _ = (loc, s) in
  failwith "TODO"

and create_gen_from_longident ~loc = function
  | Lident s -> create_gen_from_string ~loc s
  | Ldot (lg, s) ->
      AH.Expression.pexp_ident ~loc @@ H.mk_loc ~loc @@ Ldot (lg, gen_name s)
  | _ ->
      Error.case_unsupported ~loc ~case:"Ppx.Gen.create_gen_from_longident" ()

and create_gen_from_tuple ~loc elems =
  let gens = List.map (create_gen_from_core_type ~loc) elems in

  match gens with
  | [ g1 ] -> g1
  | [ g1; g2 ] -> [%expr QCheck.pair [%e g1] [%e g2]]
  | [ g1; g2; g3 ] -> [%expr QCheck.triple [%e g1] [%e g2] [%e g3]]
  | [ g1; g2; g3; g4 ] -> [%expr QCheck.quad [%e g1] [%e g2] [%e g3] [%e g4]]
  | gens ->
      let (gens, gens_name, pat) = build_nested_gens loc gens in
      let build =
        List.map (AH.Expression.pexp_lident ~loc) gens_name
        |> AH.Expression.pexp_tuple ~loc
      in
      [%expr QCheck.map (fun [%p pat] -> [%e build]) [%e gens]]

let replace_stri infos stri =
  let info = List.hd infos in
  match stri.pstr_desc with
  (*
      Pstr_type of _ * [type_declaration]

      type t = ...

      TODO: does Nonrecursive type exists ?
   *)
  | Pstr_type (_, [ x ]) ->
      let loc = H.Info.get_loc info in
      create_gen_from_td ~loc x
  (*
      Pstr_type of _ * type_declaration list

      type t = ...
      and t' = ...
  *)
  | Pstr_type (_, xs) ->
      List.iter Pp.print_type_decl xs ;
      Error.case_unsupported ~case:"Ppx.Gen.replace_stri" ()
  | _ -> assert false
