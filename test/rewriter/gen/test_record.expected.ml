include struct
  type t = { a : int; b : string } [@@gen]

  let gen_t =
    QCheck.map
      (fun (gen_0, gen_1) -> { a = gen_0; b = gen_1 })
      (QCheck.pair QCheck.int QCheck.string)
end

include struct
  type mutable_t = { mutable a : int; mutable b : string } [@@gen]

  let gen_mutable_t =
    QCheck.map
      (fun (gen_0, gen_1) -> { a = gen_0; b = gen_1 })
      (QCheck.pair QCheck.int QCheck.string)
end

include struct
  type t2 = A of t | B of { left : int; right : int } [@@gen]

  let gen_t2 =
    QCheck.oneof
      [
        QCheck.map (fun gen_0 -> A gen_0) gen_t;
        QCheck.map
          (fun (gen_0, gen_1) -> B { left = gen_0; right = gen_1 })
          (QCheck.pair QCheck.int QCheck.int);
      ]
end
