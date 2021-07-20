let my_property f x y = f x y = x + y

include struct
  let add x y = x + y [@@pbt {| my_property[int, int] |}]

  let test_add_is_my_property =
    QCheck.Test.make
      ~name:"add_is_my_property"
      (QCheck.pair QCheck.int QCheck.int)
      (fun (gen_0, gen_1) -> my_property add gen_0 gen_1)

  let () = Runner.add_tests [ test_add_is_my_property ]
end
