exception InternalError

exception SyntaxError of string

let syntax_error c = raise (SyntaxError (Format.sprintf "%c" c))

let print_exception = function
  | SyntaxError s -> Format.printf "SyntaxError (%s)\n" s
  | e -> Format.printf "InternalError (%s)\n" (Printexc.to_string e)
