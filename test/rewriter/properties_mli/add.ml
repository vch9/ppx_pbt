let add = ( + )

let rec add_2 x y =
  match (x, y) with (0, y) -> y | (x, y) -> 1 + add_2 (x - 1) y
