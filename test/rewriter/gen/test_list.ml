type t1 = string list [@@arb]

type t2 = A of string list | B of int list [@@arb]
