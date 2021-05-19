include struct
  type t1 = int [@@gen]

  let gen_t1 = Pbt.Gens.int
end

include struct
  type t2 = float [@@gen]

  let gen_t2 = Pbt.Gens.float
end

include struct
  type t3 = char [@@gen]

  let gen_t3 = Pbt.Gens.char
end

include struct
  type t4 = string [@@gen]

  let gen_t4 = Pbt.Gens.string
end

include struct
  type t5 = unit [@@gen]

  let gen_t5 = Pbt.Gens.unit
end

include struct
  type t6 = bool [@@gen]

  let gen_t6 = Pbt.Gens.bool
end
