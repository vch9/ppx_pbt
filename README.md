ppx_pbt
=========
Syntax extension for writing property based tests in OCaml code.

New syntactic constructs
--------------------------
```ocaml
let <name> <args> = <expr>
[%%pbt {| <properties> |}]
```

TODO: specify `<properties>`

Examples
---------

### add.ml

```ocaml
let add x y = x + y
[%%pbt {| commutative[int, int] |}

(* which becomes *)

let add x y = x + y
let _ =
  let open QCheck in
  let open Pbt in
  Test.make ~name:add_is_associative
	        (pair Gens.int Gens.int)
			(fun (x,y) -> Laws.commutative add x y)
```
