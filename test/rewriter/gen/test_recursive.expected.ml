include struct
  type expr =
    | Value of value
    | If of expr * expr * expr
    | Eq of expr * expr
    | Lt of expr * expr

  and value = Bool of bool | Int of int [@@arb]

  include struct
    let rec arb_expr () = arb_expr' 5

    and arb_expr' = function
      | 0 ->
          QCheck.oneof [ QCheck.map (fun arb_0 -> Value arb_0) (arb_value ()) ]
      | n ->
          QCheck.oneof
            [
              QCheck.map (fun arb_0 -> Value arb_0) (arb_value ());
              QCheck.map
                (fun (arb_0, (arb_1, arb_2)) -> If (arb_0, arb_1, arb_2))
                (QCheck.pair
                   (arb_expr' (n - 1))
                   (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1))));
              QCheck.map
                (fun (arb_0, arb_1) -> Eq (arb_0, arb_1))
                (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1)));
              QCheck.map
                (fun (arb_0, arb_1) -> Lt (arb_0, arb_1))
                (QCheck.pair (arb_expr' (n - 1)) (arb_expr' (n - 1)));
            ]

    and arb_value () =
      QCheck.oneof
        [
          QCheck.map (fun arb_0 -> Bool arb_0) QCheck.bool;
          QCheck.map (fun arb_0 -> Int arb_0) QCheck.int;
        ]

    let arb_expr = arb_expr ()

    let arb_value = arb_value ()
  end
end
