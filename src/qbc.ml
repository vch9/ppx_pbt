open Ppxlib

let expand_pbt ~ctxt _properties =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  (* TODO *)
  [ [%stri let _ = ()] ]

let extension =
  Extension.V3.declare_inline
    "pbt"
    Extension.Context.structure_item
    Ast_pattern.(single_expr_payload (estring __))
    expand_pbt

let rule = Context_free.Rule.extension extension

let () = Driver.register_transformation ~rules:[ rule ] "pbt"
