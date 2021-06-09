include struct
  type t = A | B | C [@@arb]

  let arb = QCheck.oneof [ QCheck.always A; QCheck.always B; QCheck.always C ]
end

include struct
  type t' = t = A | B | C [@@arb]

  let arb_t' =
    QCheck.oneof [ QCheck.always A; QCheck.always B; QCheck.always C ]
end
