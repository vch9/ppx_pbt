include struct
  type two = int * string [@@arb]

  let arb_two =
    QCheck.map
      (fun (arb_0, arb_1) -> (arb_0, arb_1))
      (QCheck.pair QCheck.int QCheck.string)
end

include struct
  type three = int * string * char [@@arb]

  let arb_three =
    QCheck.map
      (fun (arb_0, (arb_1, arb_2)) -> (arb_0, arb_1, arb_2))
      (QCheck.pair QCheck.int (QCheck.pair QCheck.string QCheck.char))
end

include struct
  type four = int * string * char * float [@@arb]

  let arb_four =
    QCheck.map
      (fun ((arb_0, arb_1), (arb_2, arb_3)) -> (arb_0, arb_1, arb_2, arb_3))
      (QCheck.pair
         (QCheck.pair QCheck.int QCheck.string)
         (QCheck.pair QCheck.char QCheck.float))
end

include struct
  type five = int * string * char * float * unit [@@arb]

  let arb_five =
    QCheck.map
      (fun (arb_0, ((arb_1, arb_2), (arb_3, arb_4))) ->
        (arb_0, arb_1, arb_2, arb_3, arb_4))
      (QCheck.pair
         QCheck.int
         (QCheck.pair
            (QCheck.pair QCheck.string QCheck.char)
            (QCheck.pair QCheck.float QCheck.unit)))
end

include struct
  type six = int * string * char * float * unit * unit [@@arb]

  let arb_six =
    QCheck.map
      (fun ((arb_0, (arb_1, arb_2)), (arb_3, (arb_4, arb_5))) ->
        (arb_0, arb_1, arb_2, arb_3, arb_4, arb_5))
      (QCheck.pair
         (QCheck.pair QCheck.int (QCheck.pair QCheck.string QCheck.char))
         (QCheck.pair QCheck.float (QCheck.pair QCheck.unit QCheck.unit)))
end
