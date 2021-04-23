(*------ Parse properties ------*)
let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Parser.properties Lexer.token lexbuf_pps

open Ppxlib

let replace_tests structure_item _properties =
  let tests_generated =
    let _loc = !Ast_helper.default_loc in
    match structure_item.pstr_desc with
    | Pstr_value (_, _values_bindings) -> failwith "TODO"
    (* TODO: better error management *)
    | _ -> assert false
  in
  structure_item :: tests_generated

let replace_pbt structure_item = function
  (* Structure item by construction can attach only one property *)
  | [ pbt ] ->
      Payload.extract_pbt_from_payload pbt
      |> from_string
      |> replace_tests structure_item
  (* TODO: better error management *)
  | _ -> assert false
