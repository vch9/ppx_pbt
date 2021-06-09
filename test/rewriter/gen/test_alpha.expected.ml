include struct
  type 'a t1 = 'a [@@arb]

  let arb_t1 arb_a = arb_a
end

include struct
  type 'a t2 = 'a list [@@arb]

  let arb_t2 arb_a = QCheck.list arb_a
end

include struct
  type 'a t3 = A of 'a [@@arb]

  let arb_t3 arb_a = QCheck.oneof [ QCheck.map (fun arb_0 -> A arb_0) arb_a ]
end

include struct
  type ('a, 'b) t4 = A of 'a * 'b [@@arb]

  let arb_t4 arb_a arb_b =
    QCheck.oneof
      [
        QCheck.map
          (fun (arb_0, arb_1) -> A (arb_0, arb_1))
          (QCheck.pair arb_a arb_b);
      ]
end

include struct
  type ('left, 'right) t5 = 'left * 'right [@@arb]

  let arb_t5 arb_left arb_right =
    QCheck.map
      (fun (arb_0, arb_1) -> (arb_0, arb_1))
      (QCheck.pair arb_left arb_right)
end
