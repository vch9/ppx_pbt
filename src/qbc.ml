open Ppxlib

let attribute_name = "pbt"

let expand x = [ x ]

let impl xs = xs |> List.map expand |> List.concat

let () = Driver.register_transformation ~impl "ppx_pbt"
