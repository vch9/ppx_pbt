module SomeModule = struct
  include struct
    type t = int [@@arb]

    let arb = QCheck.int
  end

  module SomeOtherModule = struct
    include struct
      type t = float [@@arb]

      let arb = QCheck.float
    end
  end
end

include struct
  type t = Int of SomeModule.t | Float of SomeModule.SomeOtherModule.t [@@arb]

  let arb =
    QCheck.oneof
      [
        QCheck.map (fun arb_0 -> Int arb_0) SomeModule.arb;
        QCheck.map (fun arb_0 -> Float arb_0) SomeModule.SomeOtherModule.arb;
      ]
end
