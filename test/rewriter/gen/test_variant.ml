type t1 = [ `A | `B of int | `C of string ] [@@arb]

type t2 = [ `A | `B of int | `C of string | `D of t2 ] [@@arb]
