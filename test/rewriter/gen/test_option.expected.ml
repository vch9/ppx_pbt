include struct
  type 'a my_option = None | Some of 'a [@@gen]

  let gen_my_option a =
    QCheck.oneof
      [
        QCheck.make @@ QCheck.Gen.return None;
        QCheck.map (fun gen_0 -> Some gen_0) a;
      ]
end

include struct
  type t1 = int my_option [@@gen]

  let gen_t1 = gen_my_option Pbt.Gens.int
end

include struct
  type t2 = int option [@@gen]

  let gen_t2 = QCheck.option Pbt.Gens.int
end
