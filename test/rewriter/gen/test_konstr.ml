type t1 = A of int [@@gen]

let pp_t1 (A i) = Printf.sprintf "A %d" i

type t2 = B of int | C of int [@@gen]

let pp_t2 = function
  | B i -> Printf.sprintf "B %d" i
  | C i -> Printf.sprintf "C %d" i

type t3 = X of t1 | Y of t2 | Z of string [@@gen]

let pp_t3 = function
  | X t1 -> Printf.sprintf "X (%s)" (pp_t1 t1)
  | Y t2 -> Printf.sprintf "Y (%s)" (pp_t2 t2)
  | Z s -> Printf.sprintf "Z %s" s

type t4 = Left | Right [@@gen]
