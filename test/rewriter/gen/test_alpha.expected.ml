include struct
  type 'a t1 = 'a [@@gen]

  let gen_t1 a = a
end

include struct
  type 'a t2 = 'a list [@@gen]

  let gen_t2 a = QCheck.list a
end

include struct
  type 'a t3 = A of 'a [@@gen]

  let gen_t3 a = QCheck.oneof [ QCheck.map (fun gen_0 -> A gen_0) a ]
end

include struct
  type ('a, 'b) t4 = A of 'a * 'b [@@gen]

  let gen_t3 a b =
    QCheck.oneof
      [ QCheck.map (fun (gen_0, gen_1) -> A (gen_0, gen_1)) (QCheck.pair a b) ]
end

include struct
  type ('left, 'right) t5 = 'left * 'right [@@gen]

  let gen_t5 left right = QCheck.pair left right
end
