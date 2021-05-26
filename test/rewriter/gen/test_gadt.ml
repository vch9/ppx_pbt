type _ expr =
  | Value : 'a value -> 'a expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Eq : 'a expr * 'a expr -> bool expr
  | Lt : int expr * int expr -> bool expr

and _ value = Bool : bool -> bool value | Int : int -> int value [@@gen]
