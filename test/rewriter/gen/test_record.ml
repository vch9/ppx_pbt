type t = { a : int; b : string } [@@arb]

type mutable_t = { mutable a : int; mutable b : string } [@@arb]

type t2 = A of t | B of { left : int; right : int } [@@arb]

(* type t3 = { left : int; right : t3 } [@@arb] *)
