include struct
  let add = ( + )

  let test_add_is_commutative =
    QCheck.Test.make
      ~name:"add_is_commutative"
      (QCheck.pair QCheck.int QCheck.int)
      (fun (gen_0, gen_1) -> Pbt.Properties.commutative add gen_0 gen_1)

  let test_add_is_associative =
    QCheck.Test.make
      ~name:"add_is_associative"
      (QCheck.pair QCheck.int (QCheck.pair QCheck.int QCheck.int))
      (fun (gen_0, (gen_1, gen_2)) ->
        Pbt.Properties.associative add gen_0 gen_1 gen_2)

  let () = Runner.add_tests [ test_add_is_commutative; test_add_is_associative ]
end

include struct
  let rec add_2 x y =
    match (x, y) with (0, y) -> y | (x, y) -> 1 + add_2 (x - 1) y

  let test_add_2_is_commutative =
    QCheck.Test.make
      ~name:"add_2_is_commutative"
      (QCheck.pair QCheck.int QCheck.int)
      (fun (gen_0, gen_1) -> Pbt.Properties.commutative add_2 gen_0 gen_1)

  let () = Runner.add_tests [ test_add_2_is_commutative ]
end

module Math = struct
  include struct
    let math_add = ( + )

    let test_math_add_is_commutative =
      QCheck.Test.make
        ~name:"math_add_is_commutative"
        (QCheck.pair QCheck.int QCheck.int)
        (fun (gen_0, gen_1) -> Pbt.Properties.commutative math_add gen_0 gen_1)

    let () = Runner.add_tests [ test_math_add_is_commutative ]
  end
end

module MathFunct (MATH : sig
  val math_add : int -> int -> int
end) =
struct
  include struct
    let math_funct_add = MATH.math_add

    let test_math_funct_add_is_commutative =
      QCheck.Test.make
        ~name:"math_funct_add_is_commutative"
        (QCheck.pair QCheck.int QCheck.int)
        (fun (gen_0, gen_1) ->
          Pbt.Properties.commutative math_funct_add gen_0 gen_1)

    let () = Runner.add_tests [ test_math_funct_add_is_commutative ]
  end
end
