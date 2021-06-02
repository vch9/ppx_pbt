type tree = Leaf | Node of int * tree * tree [@@gen]

type expr =
  | Value of int
  | If of expr * expr * expr
  | Eq of expr * expr
  | Lt of expr * expr
[@@gen]
