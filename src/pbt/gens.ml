(* Generators *)

let int = QCheck.int

let uint = QCheck.(map (fun x -> abs x) int)
