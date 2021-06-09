type expr =
  | Value of value
  | If of expr * expr * expr
  | Eq of expr * expr
  | Lt of expr * expr

and value = Bool of bool | Int of int [@@arb]
