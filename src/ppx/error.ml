exception InternalError

exception SyntaxError of string

exception CaseUnsupported of string

exception PropertyNotSupported of string

exception PropertyGeneratorsMissing of string * int * int

let syntax_error c = raise (SyntaxError (Format.sprintf "%c" c))

let print_exception = function
  | SyntaxError s -> Format.printf "SyntaxError (%s)\n" s
  | CaseUnsupported s -> Format.printf "CaseUnsupported in (%s)\n" s
  | PropertyNotSupported s ->
      Format.printf "The property %s is not supported in ppx_pbt\n" s
  | PropertyGeneratorsMissing (s, actual, expected) ->
      Format.printf
        "Property %s requires %d generators, here %d are applied\n"
        s
        actual
        expected
  | e -> Format.printf "InternalError (%s)\n" (Printexc.to_string e)
