open Ppxlib
open Error

let rec extract_pbt_from_payload = function
  | PStr structure -> extract_pbt_from_structure structure
  | _ -> raise (CaseUnsupported "extract_pbt_from_payload")

and extract_pbt_from_structure_item structure_item =
  match structure_item.pstr_desc with
  | Pstr_eval (expr, _) -> extract_pbt_from_expression expr
  | _ -> raise (CaseUnsupported "extract_pbt_from_structure_item")

and extract_pbt_from_structure structure =
  List.map extract_pbt_from_structure_item structure |> List.hd

and extract_pbt_from_expression expression =
  match expression.pexp_desc with
  | Pexp_constant constant -> extract_pbt_from_constant constant
  | _ -> raise (CaseUnsupported "extract_pbt_from_expression")

and extract_pbt_from_constant = function
  | Pconst_string (str, _, _) -> str
  | _ -> raise (CaseUnsupported "extract_pbt_from_constant")
