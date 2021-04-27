%{
%}

%token<string> ID

%token LCROCH
%token RCROCH
%token COMMA

%token EOF

%start<Properties.t> properties
%%

properties:
| p=property EOF
{ [p] }

property:
| p=ID LCROCH args=separated_list(COMMA, ID) RCROCH
{ (p, args) }
