include struct
  type t1 = int [@@gen]

  let gen_t1 = QCheck.int
end

include struct
  type t2 = float [@@gen]

  let gen_t2 = QCheck.float
end

include struct
  type t3 = char [@@gen]

  let gen_t3 = QCheck.char
end

include struct
  type t4 = string [@@gen]

  let gen_t4 = QCheck.string
end

include struct
  type t5 = unit [@@gen]

  let gen_t5 = QCheck.unit
end

include struct
  type t6 = bool [@@gen]

  let gen_t6 = QCheck.bool
end

include struct
  type t7 = int32 [@@gen]

  let gen_t7 = QCheck.int32
end

include struct
  type t8 = Int32.t [@@gen]

  let gen_t8 = QCheck.int32
end

include struct
  type t9 = int64 [@@gen]

  let gen_t9 = QCheck.int64
end

include struct
  type t10 = Int64.t [@@gen]

  let gen_t10 = QCheck.int64
end

include struct
  type t11 = Bytes.t [@@gen]

  let gen_t11 =
    QCheck.map (fun n -> Bytes.create n) QCheck.(0 -- Sys.max_string_length)
end
