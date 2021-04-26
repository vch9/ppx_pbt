open Ppxlib
open Error

(*------ Find attributes in Ast.structure_item ------*)

let attribute_name = "pbt"

let rec get_structure_item_pbt structure_item =
  match structure_item.pstr_desc with
  | Pstr_eval (expression, attributes) ->
      get_attributes_pbt expression.pexp_attributes
      @ get_attributes_pbt attributes
  | Pstr_value (_, values_binding) -> get_values_binding_pbt values_binding
  | Pstr_module module_binding ->
      let pmb_expr = module_binding.pmb_expr in
      get_module_expr_pbt pmb_expr
  | _ -> []

and get_structure_pbt structures =
  List.fold_left
    (fun acc structure -> get_structure_item_pbt structure @ acc)
    []
    structures

and get_module_expr_pbt module_expr =
  match module_expr.pmod_desc with
  | Pmod_structure structure -> get_structure_pbt structure
  | _ -> []

and get_values_binding_pbt vs_binds =
  List.fold_left
    (fun acc value_binding ->
      get_attributes_pbt value_binding.pvb_attributes @ acc)
    []
    vs_binds

and get_attributes_pbt attrs =
  List.filter_map
    (fun attribute ->
      if String.equal attribute.attr_name.txt attribute_name then
        Some attribute.attr_payload
      else None)
    attrs

let structure_item_contains_pbt structure_item =
  match get_structure_item_pbt structure_item with [] -> false | _ -> true

(*------ Replace structure item when attached with pbt attribute ------*)
let rec replace_structure_item structure_item : structure_item list =
  match structure_item.pstr_desc with
  (* -- Recursives cases -- *)
  (* module <Name> = struct .. end *)
  | Pstr_module module_binding ->
      [
        {
          structure_item with
          pstr_desc = Pstr_module (replace_module_binding module_binding);
        };
      ]
  (* -- Structures items where ppx_pbt is accepted -- *)
  (* let <fname> <args> = <expr> *)
  | Pstr_value _ ->
      (* TODO: optimize here *)
      if structure_item_contains_pbt structure_item then
        Tests.replace_pbt structure_item (get_structure_item_pbt structure_item)
      else [ structure_item ]
  | _ -> [ structure_item ]

and replace_structure structure =
  List.map replace_structure_item structure |> List.concat

and replace_module_binding module_binding =
  { module_binding with pmb_expr = replace_module_expr module_binding.pmb_expr }

and replace_module_expr module_expr =
  match module_expr.pmod_desc with
  | Pmod_structure structure ->
      {
        module_expr with
        pmod_desc = Pmod_structure (replace_structure structure);
      }
  | _ -> module_expr

let expand struct_item =
  try
    if structure_item_contains_pbt struct_item then
      replace_structure_item struct_item
    else [ struct_item ]
  with e ->
    Error.print_exception e ;
    raise InternalError

let impl xs = xs |> List.map expand |> List.concat

let () = Driver.register_transformation ~impl "ppx_pbt"
