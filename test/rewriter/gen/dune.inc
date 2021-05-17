
(rule
 (targets test_primitive.expected.format)
 (deps (:pp pp.exe) (:input test_primitive.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_primitive.actual)
 (deps (:pp pp.exe) (:input test_primitive.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_primitive.actual) (:expected test_primitive.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_primitive)
 (modules test_primitive)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))
