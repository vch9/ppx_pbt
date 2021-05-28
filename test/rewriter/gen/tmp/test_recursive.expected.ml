include struct
  type expr =
    | Value of value
    | If of expr * expr * expr
    | Eq of expr * expr
    | Lt of expr * expr

  and value = Bool of bool | Int of int [@@gen { fuel = 5 }]

  let rec gen_expr fuel =
    match fuel with
    | 0 -> QCheck.oneof [ QCheck.map (fun gen_0 -> Value gen_0) gen_value ]
    | n ->
        QCheck.oneof
          [
            QCheck.map (fun gen_0 -> Value gen_0) gen_value;
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

  and gen_value =
    QCheck.oneof
      [
        QCheck.map (fun gen_0 -> Bool gen_0) Pbt.Gens.bool;
        QCheck.map (fun gen_0 -> Int gen_0) Pbt.Gens.int;
      ]

  let gen_expr = gen_expr 5
end
