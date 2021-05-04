
(library
 (name test_associative)
 (modules test_associative)
 (libraries qbc_ppx))

(rule
 (targets test_associative.expected.format)
 (deps (:pp pp.exe) (:input test_associative.expected))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_associative.actual)
 (deps (:pp pp.exe) (:input test_associative.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_associative.actual) (:expected test_associative.expected.format))
 (action (diff %{expected} %{actual})))

(library
 (name test_commutative)
 (modules test_commutative)
 (libraries qbc_ppx))

(rule
 (targets test_commutative.expected.format)
 (deps (:pp pp.exe) (:input test_commutative.expected))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_commutative.actual)
 (deps (:pp pp.exe) (:input test_commutative.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_commutative.actual) (:expected test_commutative.expected.format))
 (action (diff %{expected} %{actual})))

(library
 (name test_local_gen)
 (modules test_local_gen)
 (libraries qbc_ppx))

(rule
 (targets test_local_gen.expected.format)
 (deps (:pp pp.exe) (:input test_local_gen.expected))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_local_gen.actual)
 (deps (:pp pp.exe) (:input test_local_gen.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_local_gen.actual) (:expected test_local_gen.expected.format))
 (action (diff %{expected} %{actual})))

(library
 (name test_neutral)
 (modules test_neutral)
 (libraries qbc_ppx))

(rule
 (targets test_neutral.expected.format)
 (deps (:pp pp.exe) (:input test_neutral.expected))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_neutral.actual)
 (deps (:pp pp.exe) (:input test_neutral.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_neutral.actual) (:expected test_neutral.expected.format))
 (action (diff %{expected} %{actual})))
