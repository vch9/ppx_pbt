let tests = ref []

let add_test t = tests := t :: !tests

let add_tests xs = List.iter add_test xs

let run () =
  let _ = QCheck_runner.run_tests ~verbose:true !tests in
  ()
