(*------ Parse properties ------*)
let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Parser.properties Lexer.token lexbuf_pps

open Ppxlib

(* TODO: better manage failwith "Else" *)

let rec get_tested_fun_values_binding values_bindings =
  List.map get_tested_fun_value_binding values_bindings |> List.hd

and get_tested_fun_value_binding value_binding =
  match value_binding.pvb_expr.pexp_desc with
  | Pexp_let (_, values_binding, _) ->
      get_tested_fun_values_binding values_binding
  | Pexp_ident longident_loc -> get_tested_fun_longident_loc longident_loc
  | _ -> failwith "Else"

and get_tested_fun_longident_loc longident_loc =
  match longident_loc.txt with Ldot (_, str) -> str | _ -> failwith "Else"

let build_pattern loc test_name =
  {
    ppat_desc = Ppat_var { txt = test_name; loc };
    ppat_loc = loc;
    ppat_loc_stack = [];
    ppat_attributes = [];
  }

let build_value_binding loc pat expr =
  { pvb_pat = pat; pvb_expr = expr; pvb_attributes = []; pvb_loc = loc }

let build_expression loc exp_desc =
  {
    pexp_desc = exp_desc;
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

let build_let loc values_binding exp =
  let let_exp = Pexp_let (Nonrecursive, values_binding, exp) in
  build_expression loc let_exp

(* Build_open _ *)
let build_open _ = failwith "TODO"

(* Build_gens _ *)
let build_gens _ = failwith "TODO"

(* Build_testing _ *)
let build_testing_fun _ = failwith "TODO"

(* Build_testing_fun _

   <build_open>
   Test.make ~name:<fun_name>_is_<name>
   <build_gens>
   <build_testing_fun> *)
let build_qcheck_tests _ = failwith "TODO"

(* Build_test loc fun_name (name, args) :

   <build_open>
   <build_qcheck_test> *)
let build_test loc _fun_name (_name, _args) =
  build_expression loc (Pexp_constant (Pconst_string ("TODO", loc, None)))

(* Build fun_name (name, args) :

   let test_<fun_name>_is_<name> = <build_test> *)
let build fun_name ((name, _args) as properties) =
  let loc = !Ast_helper.default_loc in
  let test_name = Format.sprintf "test_%s_is_%s" fun_name name in
  let vb_pat = build_pattern loc test_name in
  let test_exp = build_test loc fun_name properties in
  let value_binding = build_value_binding loc vb_pat test_exp in
  let test = Pstr_value (Nonrecursive, [ value_binding ]) in
  { pstr_loc = loc; pstr_desc = test }

let replace_tests structure_item properties =
  let tests_generated =
    match structure_item.pstr_desc with
    | Pstr_value (_, values_bindings) ->
        let fun_name = get_tested_fun_values_binding values_bindings in
        List.map (build fun_name) properties
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
