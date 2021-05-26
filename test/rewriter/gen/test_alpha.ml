type 'a t1 = 'a [@@gen]

type 'a t2 = 'a list [@@gen]

type 'a t3 = A of 'a [@@gen]

type ('a, 'b) t4 = A of 'a * 'b [@@gen]

type ('left, 'right) t5 = 'left * 'right [@@gen]
