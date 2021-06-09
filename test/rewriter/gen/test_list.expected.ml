include struct
  type t1 = string list [@@arb]

  let arb_t1 = QCheck.list QCheck.string
end

include struct
  type t2 = A of string list | B of int list [@@arb]

  let arb_t2 =
    QCheck.oneof
      [
        QCheck.map (fun arb_0 -> A arb_0) (QCheck.list QCheck.string);
        QCheck.map (fun arb_0 -> B arb_0) (QCheck.list QCheck.int);
      ]
end
