include struct
  type t1 = int [@@arb]

  let arb_t1 = QCheck.int
end

include struct
  type t2 = float [@@arb]

  let arb_t2 = QCheck.float
end

include struct
  type t3 = char [@@arb]

  let arb_t3 = QCheck.char
end

include struct
  type t4 = string [@@arb]

  let arb_t4 = QCheck.string
end

include struct
  type t5 = unit [@@arb]

  let arb_t5 = QCheck.unit
end

include struct
  type t6 = bool [@@arb]

  let arb_t6 = QCheck.bool
end

include struct
  type t7 = int32 [@@arb]

  let arb_t7 = QCheck.int32
end

include struct
  type t8 = Int32.t [@@arb]

  let arb_t8 = QCheck.int32
end

include struct
  type t9 = int64 [@@arb]

  let arb_t9 = QCheck.int64
end

include struct
  type t10 = Int64.t [@@arb]

  let arb_t10 = QCheck.int64
end

include struct
  type t11 = Bytes.t [@@arb]

  let arb_t11 =
    QCheck.map (fun n -> Bytes.create n) QCheck.(0 -- Sys.max_string_length)
end
