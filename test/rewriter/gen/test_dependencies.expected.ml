module SomeModule = struct
  include struct
    type t = int [@@gen]

    let gen = QCheck.int
  end

  module SomeOtherModule = struct
    include struct
      type t = float [@@gen]

      let gen = QCheck.float
    end
  end
end

include struct
  type t = Int of SomeModule.t | Float of SomeModule.SomeOtherModule.t [@@gen]

  let gen =
    QCheck.oneof
      [
        QCheck.map (fun gen_0 -> Int gen_0) SomeModule.gen;
        QCheck.map (fun gen_0 -> Float gen_0) SomeModule.SomeOtherModule.gen;
      ]
end
