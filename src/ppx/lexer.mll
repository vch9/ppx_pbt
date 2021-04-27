{
  open Parser
  open Error
}


let blank = [' ' '\t' '\n']
let id = ['a'-'z' 'A'-'Z']+['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
(* Layout *)
| blank+ { token lexbuf }

(* Punctuations *)
| "[" { LCROCH }
| "]" { RCROCH }
| "," { COMMA }

(* Regexp *)
| id as s { ID s }

| eof { EOF }

| _ as c { syntax_error c }