(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
