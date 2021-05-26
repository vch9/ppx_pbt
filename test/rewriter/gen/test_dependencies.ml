module SomeModule = struct
  type t = int [@@gen]

  module SomeOtherModule = struct
    type t = float [@@gen]
  end
end

type t = Int of SomeModule.t | Float of SomeModule.SomeOtherModule.t [@@gen]
