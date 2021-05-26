type t1 = string list [@@gen]

type t2 = A of string list | B of int list [@@gen]

type t3 = { my_list : int list } [@@gen]
