include struct
  let add x y = x + y [@@pbt {| associative[int, int, int] |}]

  let test_add_is_associative =
    QCheck.Test.make
      ~name:"add_is_associative"
      (QCheck.pair Pbt.Gens.int (QCheck.pair Pbt.Gens.int Pbt.Gens.int))
      (fun (gen_0, (gen_1, gen_2)) ->
        Pbt.Properties.associative add gen_0 gen_1 gen_2)

  let () = Runner.add_tests [ test_add_is_associative ]
end
