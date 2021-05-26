include struct
  type tree = Leaf | Node of int * tree * tree [@@gen { fuel = 5 }]

  let rec gen_tree fuel =
    match fuel with
    | 0 -> QCheck.oneof [ QCheck.make @@ QCheck.Gen.return Leaf ]
    | n ->
        QCheck.oneof
          [
            QCheck.make @@ QCheck.Gen.return Leaf;
            QCheck.map
              (fun (gen_0, (gen_1, gen_2)) -> Node (gen_0, gen_1, gen_2))
              (QCheck.pair
                 Pbt.Gens.int
                 (QCheck.pair (gen_tree (n - 1)) (gen_tree (n - 1))));
          ]

  let gen_tree = gen_tree 5
end
