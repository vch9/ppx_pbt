%{
%}

%token<string> ID

%token LCROCH
%token RCROCH
%token COMMA
(* %token SEMICOLON *)

%start<Properties.t> properties
%%

properties:
| p=property
{ [p] }

property:
| p=ID LCROCH args=separated_list(COMMA, ID) RCROCH
{ (p, args) }
