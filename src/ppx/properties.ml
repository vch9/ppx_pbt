type property_name = string

type arg = string

type property = property_name * arg list

type properties = property list

and t = properties

(* TODO: change to formatted version *)
let rec properties_to_str = function
  | [ p ] -> Format.sprintf "%s\n" (property_to_str p)
  | p1 :: p2 :: pps ->
      Format.sprintf
        "%s ;\n%s"
        (property_to_str p1)
        (properties_to_str (p2 :: pps))
  | [] -> assert false

and property_to_str (name, args) =
  Format.sprintf "%s[%s]" name (args_to_str args)

and args_to_str = function
  | [] -> ""
  | [ arg ] -> arg
  | arg1 :: arg2 :: args ->
      Format.sprintf "%s ; %s" arg1 (args_to_str (arg2 :: args))

let properties_gens = [ ("commutative", 2) ]

let rec takes_n list n =
  match (list, n) with
  | (_, 0) -> []
  | (x :: xs, n) -> x :: takes_n xs (n - 1)
  (* Not enough elements in the list:
     not enough generators for the property
     TODO: generate human readable error *)
  | _ -> failwith "TODO ERROR"

let get_gens property_name args =
  match List.assoc_opt property_name properties_gens with
  | Some n -> takes_n args n
  (* A design choice must be choose here,
     - reject unknown property ?
     - inline it as a call to a function ? *)
  | None -> failwith "TODO ERROR"
