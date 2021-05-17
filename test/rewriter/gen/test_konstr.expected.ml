type t1 = A of int [@@gen]

let gen_t1 = QCheck.oneof [ QCheck.map (fun gen_0 -> A gen_0) Pbt.Gens.int ]

let pp_t1 (A i) = Printf.sprintf "A %d" i

type t2 = B of int | C of int [@@gen]

let gen_t2 =
  QCheck.oneof
    [
      QCheck.map (fun gen_0 -> B gen_0) Pbt.Gens.int;
      QCheck.map (fun gen_0 -> C gen_0) Pbt.Gens.int;
    ]

let pp_t2 = function
  | B i -> Printf.sprintf "B %d" i
  | C i -> Printf.sprintf "C %d" i

type t3 = X of t1 | Y of t2 | Z of string [@@gen]

let gen_t3 =
  QCheck.oneof
    [
      QCheck.map (fun gen_0 -> X gen_0) gen_t1;
      QCheck.map (fun gen_0 -> Y gen_0) gen_t2;
      QCheck.map (fun gen_0 -> Z gen_0) Pbt.Gens.string;
    ]

let pp_t3 = function
  | X t1 -> Printf.sprintf "X (%s)" (pp_t1 t1)
  | Y t2 -> Printf.sprintf "Y (%s)" (pp_t2 t2)
  | Z s -> Printf.sprintf "Z %s" s
