type 'a t1 = 'a [@@arb]

type 'a t2 = 'a list [@@arb]

type 'a t3 = A of 'a [@@arb]

type ('a, 'b) t4 = A of 'a * 'b [@@arb]

type ('left, 'right) t5 = 'left * 'right [@@arb]
