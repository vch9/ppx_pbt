%{
%}

%token<string> ID

%token LCROCH
%token RCROCH
%token LBRACKET
%token RBRACKET
%token COMMA
%token SEMICOLON

%token EOF

%start<Properties.t> properties
%%

properties:
| ps=separated_nonempty_list(SEMICOLON, property) EOF
{ ps }

property:
| p=ID x=args y=gens
{ (p, x, y) }

args:
| LBRACKET args=separated_nonempty_list(COMMA, ID) RBRACKET
{ args }
| { [] }

gens:
| LCROCH gens=separated_nonempty_list(COMMA, ID) RCROCH
{ gens }
| { [] }
