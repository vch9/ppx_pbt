include struct
  type two = int * string [@@gen]

  let gen_two = QCheck.pair Pbt.Gens.int Pbt.Gens.string
end

include struct
  type three = int * string * char [@@gen]

  let gen_three = QCheck.triple Pbt.Gens.int Pbt.Gens.string Pbt.Gens.char
end

include struct
  type four = int * string * char * float [@@gen]

  let gen_four =
    QCheck.quad Pbt.Gens.int Pbt.Gens.string Pbt.Gens.char Pbt.Gens.float
end

include struct
  type five = int * string * char * float * unit [@@gen]

  let gen_five =
    QCheck.map
      (fun (gen_0, ((gen_1, gen_2), (gen_3, gen_4))) ->
        (gen_0, gen_1, gen_2, gen_3, gen_4))
      (QCheck.pair
         Pbt.Gens.int
         (QCheck.pair
            (QCheck.pair Pbt.Gens.string Pbt.Gens.char)
            (QCheck.pair Pbt.Gens.float Pbt.Gens.unit)))
end

include struct
  type six = int * string * char * float * unit * unit [@@gen]

  let gen_six =
    QCheck.map
      (fun ((gen_0, (gen_1, gen_2)), (gen_3, (gen_4, gen_5))) ->
        (gen_0, gen_1, gen_2, gen_3, gen_4, gen_5))
      (QCheck.pair
         (QCheck.pair Pbt.Gens.int (QCheck.pair Pbt.Gens.string Pbt.Gens.char))
         (QCheck.pair Pbt.Gens.float (QCheck.pair Pbt.Gens.unit Pbt.Gens.unit)))
end
