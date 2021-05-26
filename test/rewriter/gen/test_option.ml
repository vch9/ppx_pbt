type 'a my_option = None | Some of 'a [@@gen]

type t1 = int my_option [@@gen]

type t2 = int option [@@gen]
