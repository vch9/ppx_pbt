include struct
  type 'a t1 = 'a [@@gen]

  let gen_t1 gen_a = gen_a
end

include struct
  type 'a t2 = 'a list [@@gen]

  let gen_t2 gen_a = Pbt.Gens.list gen_a
end

include struct
  type 'a t3 = A of 'a [@@gen]

  let gen_t3 gen_a = QCheck.oneof [ QCheck.map (fun gen_0 -> A gen_0) gen_a ]
end

include struct
  type ('a, 'b) t4 = A of 'a * 'b [@@gen]

  let gen_t4 gen_a gen_b =
    QCheck.oneof
      [
        QCheck.map
          (fun (gen_0, gen_1) -> A (gen_0, gen_1))
          (QCheck.pair gen_a gen_b);
      ]
end

include struct
  type ('left, 'right) t5 = 'left * 'right [@@gen]

  let gen_t5 gen_left gen_right = QCheck.pair gen_left gen_right
end
