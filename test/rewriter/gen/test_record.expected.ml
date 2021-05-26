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
