{
  open Parser
  open Error
}


let blank = [' ' '\009']
let id = ['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
(* Layout *)
| blank+ { token lexbuf }

(* Punctuations *)
| "[" { LCROCH }
| "]" { RCROCH }

(* Regexp *)
| id as s { ID s }

| _+ as s { syntax_error s }