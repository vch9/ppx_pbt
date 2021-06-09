include struct
  type tree = Leaf | Node of int * tree * tree [@@arb]

  include struct
    let rec arb_tree () = arb_tree' 5

    and arb_tree' = function
      | 0 -> QCheck.oneof [ QCheck.always Leaf ]
      | n ->
          QCheck.oneof
            [
              QCheck.always Leaf;
              QCheck.map
                (fun (arb_0, (arb_1, arb_2)) -> Node (arb_0, arb_1, arb_2))
                (QCheck.pair
                   QCheck.int
                   (QCheck.pair (arb_tree' (n - 1)) (arb_tree' (n - 1))));
            ]

    let arb_tree = arb_tree ()
  end
end

include struct
  type expr =
    | Value of int
    | If of expr * expr * expr
    | Eq of expr * expr
    | Lt of expr * expr
  [@@arb]

  include struct
    let rec arb_expr () = arb_expr' 5

    and arb_expr' = function
      | 0 -> QCheck.oneof [ QCheck.map (fun arb_0 -> Value arb_0) QCheck.int ]
      | n ->
          QCheck.oneof
            [
              QCheck.map (fun arb_0 -> Value arb_0) QCheck.int;
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

    let arb_expr = arb_expr ()
  end
end
