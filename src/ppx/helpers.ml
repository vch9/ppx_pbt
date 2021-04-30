open Ppxlib

(* --- Building patterns --- *)
let build_pattern loc pat =
  { ppat_desc = pat; ppat_loc = loc; ppat_loc_stack = []; ppat_attributes = [] }

let build_pattern_var loc x = build_pattern loc @@ Ppat_var { txt = x; loc }

let build_pattern_default loc = build_pattern loc @@ Ppat_any

(* --- Building expressions --- *)
let build_expression loc exp =
  { pexp_desc = exp; pexp_loc = loc; pexp_loc_stack = []; pexp_attributes = [] }

let build_value_binding loc pat expr =
  { pvb_pat = pat; pvb_expr = expr; pvb_attributes = []; pvb_loc = loc }

let build_let loc values_binding exp =
  build_expression loc @@ Pexp_let (Nonrecursive, values_binding, exp)

let build_string loc str =
  build_expression loc (Pexp_constant (Pconst_string (str, loc, None)))

let rec build_list loc = function
  | [] -> [%expr []]
  | x :: xs -> [%expr [%e x] :: [%e build_list loc xs]]

let build_apply loc to_apply args =
  build_expression loc @@ Pexp_apply (to_apply, args)

let build_ident loc x =
  build_expression loc @@ Pexp_ident { txt = Lident x; loc }

let default_expr loc = build_string loc "TODO"
