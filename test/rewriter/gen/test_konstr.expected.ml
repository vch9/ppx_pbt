include struct
  type t1 = A of int [@@arb]

  let arb_t1 = QCheck.oneof [ QCheck.map (fun arb_0 -> A arb_0) QCheck.int ]
end

let pp_t1 (A i) = Printf.sprintf "A %d" i

include struct
  type t2 = B of int | C of int [@@arb]

  let arb_t2 =
    QCheck.oneof
      [
        QCheck.map (fun arb_0 -> B arb_0) QCheck.int;
        QCheck.map (fun arb_0 -> C arb_0) QCheck.int;
      ]
end

let pp_t2 = function
  | B i -> Printf.sprintf "B %d" i
  | C i -> Printf.sprintf "C %d" i

include struct
  type t3 = X of t1 | Y of t2 | Z of string [@@arb]

  let arb_t3 =
    QCheck.oneof
      [
        QCheck.map (fun arb_0 -> X arb_0) arb_t1;
        QCheck.map (fun arb_0 -> Y arb_0) arb_t2;
        QCheck.map (fun arb_0 -> Z arb_0) QCheck.string;
      ]
end

let pp_t3 = function
  | X t1 -> Printf.sprintf "X (%s)" (pp_t1 t1)
  | Y t2 -> Printf.sprintf "Y (%s)" (pp_t2 t2)
  | Z s -> Printf.sprintf "Z %s" s

include struct
  type t4 = Left | Right [@@arb]

  let arb_t4 = QCheck.oneof [ QCheck.always Left; QCheck.always Right ]
end

include struct
  type t5 = Simple of int | Double of int * int | Triple of int * int * int
  [@@arb]

  let arb_t5 =
    QCheck.oneof
      [
        QCheck.map (fun arb_0 -> Simple arb_0) QCheck.int;
        QCheck.map
          (fun (arb_0, arb_1) -> Double (arb_0, arb_1))
          (QCheck.pair QCheck.int QCheck.int);
        QCheck.map
          (fun (arb_0, (arb_1, arb_2)) -> Triple (arb_0, arb_1, arb_2))
          (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int));
      ]
end
