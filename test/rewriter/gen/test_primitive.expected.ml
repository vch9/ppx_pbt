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
