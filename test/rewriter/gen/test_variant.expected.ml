include struct
  type t1 = [ `A | `B of int | `C of string ] [@@arb]

  let arb_t1 =
    (QCheck.oneof
       [
         QCheck.always `A;
         QCheck.map (fun arb_0 -> `B arb_0) QCheck.int;
         QCheck.map (fun arb_0 -> `C arb_0) QCheck.string;
       ]
      : t1 QCheck.arbitrary)
end

include struct
  type t2 = [ `A | `B of int | `C of string | `D of t2 ] [@@arb]

  include struct
    let rec arb_t2 () = arb_t2' 5

    and arb_t2' = function
      | 0 ->
          (QCheck.oneof
             [
               QCheck.always `A;
               QCheck.map (fun arb_0 -> `B arb_0) QCheck.int;
               QCheck.map (fun arb_0 -> `C arb_0) QCheck.string;
             ]
            : t2 QCheck.arbitrary)
      | n ->
          (QCheck.oneof
             [
               QCheck.always `A;
               QCheck.map (fun arb_0 -> `B arb_0) QCheck.int;
               QCheck.map (fun arb_0 -> `C arb_0) QCheck.string;
               QCheck.map (fun arb_0 -> `D arb_0) (arb_t2' (n - 1));
             ]
            : t2 QCheck.arbitrary)

    let arb_t2 = arb_t2 ()
  end
end
