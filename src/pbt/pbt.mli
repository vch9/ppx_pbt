module Gens : sig
  (** Int generator *)
  val int : int QCheck.arbitrary

  (** Unsigned int generator *)
  val uint : int QCheck.arbitrary
end

module Properties : sig
  (** [commutative op x y] test commutativity law *)
  val commutative : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [associative f x y z] test associativity law *)
  val associative : ('a -> 'a -> 'a) -> 'a -> 'a -> 'a -> bool

  (** [neutral_left f elt x] test if elt is neutral on left *)
  val neutral_left : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [neutral_right f elt x] test if elt is neutral on right *)
  val neutral_right : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [neutrals f elt x] test if elt is neutral on both side *)
  val neutrals : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [capped_left f cap x] test if the function stays capped when
      the left argument is capped. *)
  val capped_left : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [capped_right f cap x] test if the function stays capped when
      the right argument is capped. *)
  val capped_right : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [capped_right f cap x] test if the function stays capped when
      the right or left argument is capped. *)
  val capped : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [eq_res f oracle x y] test if the result between f and the oracle are equals *)
  val eq_res : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [absorb_left f absorb x] test if the function returns x when
      the left argument is absorb. *)
  val absorb_left : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [absorb_right f absorb x] test if the function returns x when
      the right argument is absorb. *)
  val absorb_right : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [absorb_right f absorb x] test if the function returns x when
      the right or left argument is absorb. *)
  val absorbs : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [floored_left f floor x y] test if the function stays floored when
      the left argument is floored. *)
  val floored_left : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [floored_right f floor x y] test if the function stays floored when
      the right argument is floored. *)
  val floored_right : ('a -> 'a -> 'a) -> 'a -> 'a -> bool

  (** [floored_right f floor x y] test if the function stays floored when
      the right or left argument is floored. *)
  val floored : ('a -> 'a -> 'a) -> 'a -> 'a -> bool
end
