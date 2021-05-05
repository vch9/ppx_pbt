(* Properties *)
let commutative f x y = f x y = f y x

let associative f x y z = f (f x y) z = f x (f y z)

let neutral_left f neutral x = f neutral x = x

let neutral_right f neutral x = f x neutral = x

let neutrals f neutral x = neutral_right f neutral x && neutral_left f neutral x

let capped f cap x y = if x >= cap || y >= cap then f x y >= cap else true

let eq_res f oracle x y = f x y = oracle x y

let zero_left f zero x = f zero x = zero

let zero_right f zero x = f x zero = zero

let zeros f zero x = zero_left f zero x && zero_right f zero x

let floored_left f floor x = f floor x = floor
