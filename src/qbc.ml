open Ppxlib

let attribute_name = "pbt"

(*------ Find attributes in Ast.structure_item ------*)

let rec structure_item_contains_pbt structure_item =
  match structure_item.pstr_desc with
  | Pstr_eval (expression, attributes) ->
      attributes_contains_pbt expression.pexp_attributes
      || attributes_contains_pbt attributes
  | Pstr_value (_, values_binding) -> values_binding_contains_pbt values_binding
  | Pstr_module module_binding ->
      let pmb_expr = module_binding.pmb_expr in
      module_expr_contains_pbt pmb_expr
  | _ -> false

and structure_contains_pbt structures =
  List.exists structure_item_contains_pbt structures

and module_expr_contains_pbt module_expr =
  match module_expr.pmod_desc with
  | Pmod_structure structure -> structure_contains_pbt structure
  | _ -> false

and values_binding_contains_pbt vs_binds =
  List.exists
    (fun value_binding -> attributes_contains_pbt value_binding.pvb_attributes)
    vs_binds

and attributes_contains_pbt attrs =
  List.exists
    (fun attribute -> String.equal attribute.attr_name.txt attribute_name)
    attrs

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
  | Pstr_value (_, _values_binding) -> failwith "TODO"
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
  if structure_item_contains_pbt struct_item then
    replace_structure_item struct_item
  else [ struct_item ]

let impl xs = xs |> List.map expand |> List.concat

let () = Driver.register_transformation ~impl "ppx_pbt"
