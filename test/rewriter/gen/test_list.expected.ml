include struct
  type t1 = string list [@@gen]

  let gen_t1 = QCheck.list QCheck.string
end

include struct
  type t2 = A of string list | B of int list [@@gen]

  let gen_t2 =
    QCheck.oneof
      [
        QCheck.map (fun gen_0 -> A gen_0) (QCheck.list QCheck.string);
        QCheck.map (fun gen_0 -> B gen_0) (QCheck.list QCheck.int);
      ]
end
