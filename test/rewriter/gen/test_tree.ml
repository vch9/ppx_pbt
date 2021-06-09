type tree = Leaf | Node of int * tree * tree [@@arb]

type expr =
  | Value of int
  | If of expr * expr * expr
  | Eq of expr * expr
  | Lt of expr * expr
[@@arb]
