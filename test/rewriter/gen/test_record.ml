type t = { a : int; b : string } [@@gen]

type mutable_t = { mutable a : int; mutable b : string } [@@gen]

type t2 = A of t | B of { left : int; right : int } [@@gen]

(* type t3 = { left : int; right : t3 } [@@gen] *)
