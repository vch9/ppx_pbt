type 'a my_option = None | Some of 'a [@@arb]

type t1 = int my_option [@@arb]

type t2 = int option [@@arb]
