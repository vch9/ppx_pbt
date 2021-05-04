(* Properties *)
let commutative f x y = f x y = f y x

let associative f x y z = f (f x y) z = f x (f y z)

let neutral_left f neutral x = f neutral x = x

let neutral_right f neutral x = f x neutral = x

let neutrals f neutral x = neutral_right f neutral x && neutral_left f neutral x
