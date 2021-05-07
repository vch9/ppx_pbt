(*------ Parse properties ------*)
let from_string properties =
  let lexbuf_pps = Lexing.from_string properties in
  Parser.properties Lexer.token lexbuf_pps

open Ppxlib
open Error

let get_tested_fun_pattern pattern =
  match pattern.ppat_desc with
  | Ppat_var { txt = str; _ } -> Some str
  | _ -> None

(* Extract fun name we want to test *)
let rec get_tested_fun_values_binding values_bindings =
  (* A structured item should contains only one pbt attribute *)
  List.map get_tested_fun_value_binding values_bindings |> List.hd

and get_tested_fun_expression_desc expr_desc =
  match expr_desc with
  | Pexp_let (_, values_binding, _) ->
      get_tested_fun_values_binding values_binding
  | Pexp_ident longident_loc -> get_tested_fun_longident_loc longident_loc
  | _ -> raise (CaseUnsupported "get_tested_fun_expression_desc")

and get_tested_fun_value_binding value_binding =
  match get_tested_fun_pattern value_binding.pvb_pat with
  (* In case of let f <pattern> = <expr>, the function name is located inside
     the value_binding.pattern *)
  | Some str -> str
  (* Otherwise, we look for the function name in the expression *)
  | None -> get_tested_fun_expression_desc value_binding.pvb_expr.pexp_desc

and get_tested_fun_longident_loc longident_loc =
  match longident_loc.txt with
  | Ldot (_, str) -> str
  | _ -> raise (CaseUnsupported "get_tested_fun_longident_loc")

(* Build_gens loc properties

   [gen1] -> gen1
   [gen1; gen2] -> (pair gen1 gen2)
   [gen1; gen2; gen3] -> (pair gen1 (pair gen2 gen3))
   ...
 *)
let build_gens loc (name, _args, gens) =
  let _ = Properties.check_gens name gens in
  let gens = Gens.replace_gens loc gens in
  let nested_gens = Gens.nest_generators gens in
  (Gens.nested_pairs_to_expr loc nested_gens, nested_gens)

(* Build_testing _ *)
let build_testing_fun loc nested_gens fun_name (name, args, _) =
  let _ = Properties.check_args name args in
  let (fun_pattern, gens) = Properties.pattern_from_gens loc nested_gens in
  let call_property =
    Properties.call_property loc fun_name (name, args, gens)
  in
  [%expr fun [%p fun_pattern] -> [%e call_property]]

(* Build_test loc fun_name properties

   <build_gens>
   <build_testing_fun> *)
let build_test loc fun_name properties =
  let (gens_exp, nested_gens) = build_gens loc properties in
  let property_exp = build_testing_fun loc nested_gens fun_name properties in
  (gens_exp, property_exp)

(* Build fun_name (name, args) :

   let test_<fun_name>_is_<name> = QCheck.Test.make ~name:<name> <build_test> *)
let build fun_name ((name, _args, _gens) as properties) =
  let loc = !Ast_helper.default_loc in
  let test_name = Format.sprintf "test_%s_is_%s" fun_name name in
  let test_name_var = Helpers.build_pattern_var loc test_name in
  let qcheck_name =
    Helpers.build_string loc @@ Format.sprintf "%s_is_%s" fun_name name
  in
  let (gens, test) = build_test loc fun_name properties in
  let qcheck_test =
    [%stri
      let [%p test_name_var] =
        QCheck.Test.make ~name:[%e qcheck_name] [%e gens] [%e test]]
  in

  (test_name, qcheck_test)

let exec_tests tests_names =
  let loc = !Ast_helper.default_loc in
  let tests =
    Properties.args_to_expr loc tests_names
    |> List.map snd |> Helpers.build_list loc
  in
  [%stri let _ = QCheck_runner.run_tests ~verbose:true [%e tests]]

let replace_tests structure_item properties =
  let tests_generated =
    match structure_item.pstr_desc with
    | Pstr_value (_, values_bindings) ->
        let fun_name = get_tested_fun_values_binding values_bindings in
        let tests = List.map (build fun_name) properties in
        List.map snd tests @ [ exec_tests (List.map fst tests) ]
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
