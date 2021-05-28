module SomeModule = struct
  include struct
    type t = int [@@gen]

    let gen_t = Pbt.Gens.int
  end

  module SomeOtherModule = struct
    include struct
      type t = float [@@gen]

      let gen_t = Pbt.Gens.float
    end
  end
end

include struct
  type t = Int of SomeModule.t | Float of SomeModule.SomeOtherModule.t [@@gen]

  let gen_t =
    QCheck.oneof
      [
        QCheck.map (fun gen_0 -> Int gen_0) SomeModule.gen_t;
        QCheck.map (fun gen_0 -> Float gen_0) SomeModule.SomeOtherModule.gen_t;
      ]
end
