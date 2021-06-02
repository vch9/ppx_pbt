include struct
  type tree = Leaf | Node of int * tree * tree [@@gen]

  include struct
    let rec gen_tree = function
      | 0 -> QCheck.oneof [ QCheck.make @@ QCheck.Gen.return Leaf ]
      | n ->
          QCheck.oneof
            [
              QCheck.make @@ QCheck.Gen.return Leaf;
              QCheck.map
                (fun (gen_0, (gen_1, gen_2)) -> Node (gen_0, gen_1, gen_2))
                (QCheck.pair
                   QCheck.int
                   (QCheck.pair (gen_tree (n - 1)) (gen_tree (n - 1))));
            ]

    let gen_tree = gen_tree 5
  end
end

include struct
  type expr =
    | Value of int
    | If of expr * expr * expr
    | Eq of expr * expr
    | Lt of expr * expr
  [@@gen]

  include struct
    let rec gen_expr = function
      | 0 -> QCheck.oneof [ QCheck.map (fun gen_0 -> Value gen_0) QCheck.int ]
      | n ->
          QCheck.oneof
            [
              QCheck.map (fun gen_0 -> Value gen_0) QCheck.int;
              QCheck.map
                (fun (gen_0, (gen_1, gen_2)) -> If (gen_0, gen_1, gen_2))
                (QCheck.pair
                   (gen_expr (n - 1))
                   (QCheck.pair (gen_expr (n - 1)) (gen_expr (n - 1))));
              QCheck.map
                (fun (gen_0, gen_1) -> Eq (gen_0, gen_1))
                (QCheck.pair (gen_expr (n - 1)) (gen_expr (n - 1)));
              QCheck.map
                (fun (gen_0, gen_1) -> Lt (gen_0, gen_1))
                (QCheck.pair (gen_expr (n - 1)) (gen_expr (n - 1)));
            ]

    let gen_expr = gen_expr 5
  end
end
