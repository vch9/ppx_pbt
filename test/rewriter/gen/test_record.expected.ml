include struct
  type t = { a : int; b : string } [@@arb]

  let arb =
    QCheck.map
      (fun (arb_0, arb_1) -> { a = arb_0; b = arb_1 })
      (QCheck.pair QCheck.int QCheck.string)
end

include struct
  type mutable_t = { mutable a : int; mutable b : string } [@@arb]

  let arb_mutable_t =
    QCheck.map
      (fun (arb_0, arb_1) -> { a = arb_0; b = arb_1 })
      (QCheck.pair QCheck.int QCheck.string)
end

include struct
  type t2 = A of t | B of { left : int; right : int } [@@arb]

  let arb_t2 =
    QCheck.oneof
      [
        QCheck.map (fun arb_0 -> A arb_0) arb;
        QCheck.map
          (fun (arb_0, arb_1) -> B { left = arb_0; right = arb_1 })
          (QCheck.pair QCheck.int QCheck.int);
      ]
end
