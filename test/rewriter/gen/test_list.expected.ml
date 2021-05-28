include struct
  type t1 = string list [@@gen]

  let gen_t1 = Pbt.Gens.list Pbt.Gens.string
end

include struct
  type t2 = A of string list | B of int list [@@gen]

  let gen_t2 =
    QCheck.oneof
      [
        QCheck.map (fun gen_0 -> A gen_0) (Pbt.Gens.list Pbt.Gens.string);
        QCheck.map (fun gen_0 -> B gen_0) (Pbt.Gens.list Pbt.Gens.int);
      ]
end
