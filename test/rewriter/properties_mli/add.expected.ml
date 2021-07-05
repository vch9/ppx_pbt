let add = ( + )

let test_add_is_commutative =
  QCheck.Test.make
    ~name:"test_add_is_commutative"
    (QCheck.pair QCheck.int QCheck.int)
    (fun (gen_0, gen_1) -> Pbt.Properties.commutative add gen_0 gen_1)

let () = Runner.add_tests [ test_add_is_commutative ]
