module Gens = struct
  let int = QCheck.int

  let uint = QCheck.(map (fun x -> abs x) int)
end

module Properties = struct
  let commutative f x y = f x y = f y x

  let associative f x y z = f (f x y) z = f x (f y z)

  let neutral_left f elt x = f elt x = x

  let neutral_right f elt x = f x elt = x

  let neutrals f neutral x =
    neutral_right f neutral x && neutral_left f neutral x

  let capped_left f cap x = f cap x = cap

  let capped_right f cap x = f x cap = cap

  let capped f cap x = capped_left f cap x && capped_right f cap x

  let eq_res f oracle x y = f x y = oracle x y

  let zero_left f zero x = f zero x = zero

  let zero_right f zero x = f x zero = zero

  let zeros f zero x = zero_left f zero x && zero_right f zero x

  let floored_left f floor x = f floor x = floor

  let floored_right f floor x = f x floor = floor

  let floored f floor x = floored_left f floor x && floored_right f floor x
end
