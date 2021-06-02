include struct
  type two = int * string [@@gen]

  let gen_two =
    QCheck.map
      (fun (gen_0, gen_1) -> (gen_0, gen_1))
      (QCheck.pair QCheck.int QCheck.string)
end

include struct
  type three = int * string * char [@@gen]

  let gen_three =
    QCheck.map
      (fun (gen_0, (gen_1, gen_2)) -> (gen_0, gen_1, gen_2))
      (QCheck.pair QCheck.int (QCheck.pair QCheck.string QCheck.char))
end

include struct
  type four = int * string * char * float [@@gen]

  let gen_four =
    QCheck.map
      (fun ((gen_0, gen_1), (gen_2, gen_3)) -> (gen_0, gen_1, gen_2, gen_3))
      (QCheck.pair
         (QCheck.pair QCheck.int QCheck.string)
         (QCheck.pair QCheck.char QCheck.float))
end

include struct
  type five = int * string * char * float * unit [@@gen]

  let gen_five =
    QCheck.map
      (fun (gen_0, ((gen_1, gen_2), (gen_3, gen_4))) ->
        (gen_0, gen_1, gen_2, gen_3, gen_4))
      (QCheck.pair
         QCheck.int
         (QCheck.pair
            (QCheck.pair QCheck.string QCheck.char)
            (QCheck.pair QCheck.float QCheck.unit)))
end

include struct
  type six = int * string * char * float * unit * unit [@@gen]

  let gen_six =
    QCheck.map
      (fun ((gen_0, (gen_1, gen_2)), (gen_3, (gen_4, gen_5))) ->
        (gen_0, gen_1, gen_2, gen_3, gen_4, gen_5))
      (QCheck.pair
         (QCheck.pair QCheck.int (QCheck.pair QCheck.string QCheck.char))
         (QCheck.pair QCheck.float (QCheck.pair QCheck.unit QCheck.unit)))
end
