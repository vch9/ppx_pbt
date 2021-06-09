include struct
  type 'a my_option = None | Some of 'a [@@arb]

  let arb_my_option arb_a =
    QCheck.oneof
      [ QCheck.always None; QCheck.map (fun arb_0 -> Some arb_0) arb_a ]
end

include struct
  type t1 = int my_option [@@arb]

  let arb_t1 = arb_my_option QCheck.int
end

include struct
  type t2 = int option [@@arb]

  let arb_t2 = QCheck.option QCheck.int
end
