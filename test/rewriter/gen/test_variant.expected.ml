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

(* include struct
 *   type t2 = [ `A | `B of int | `C of string | `D of t2 ] [@@arb]
 *
 *   let rec arb_t2 () = arb_t2' 5
 *
 *   and arb_t2' = function
 *     | 0 ->
 *         QCheck.oneof
 *           [
 *             QCheck.always `A;
 *             QCheck.map (fun gen_0 -> `B gen_0) QCheck.int;
 *             QCheck.map (fun gen_0 -> `C gen_0) QCheck.string;
 *           ]
 *     | n ->
 *         QCheck.oneof
 *           [
 *             QCheck.always `A;
 *             QCheck.map (fun gen_0 -> `B gen_0) QCheck.int;
 *             QCheck.map (fun gen_0 -> `C gen_0) QCheck.string;
 *             QCheck.map (fun gen_0 -> `D gen_0) (arb_t2' (n - 1));
 *           ]
 *
 *   let arb_t2 = arb_t2 ()
 * end *)
