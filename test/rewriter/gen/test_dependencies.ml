module SomeModule = struct
  type t = int [@@arb]

  module SomeOtherModule = struct
    type t = float [@@arb]
  end
end

type t = Int of SomeModule.t | Float of SomeModule.SomeOtherModule.t [@@arb]
