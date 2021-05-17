
(rule
 (targets test_konstr.expected.format)
 (deps (:pp pp.exe) (:input test_konstr.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_konstr.actual)
 (deps (:pp pp.exe) (:input test_konstr.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_konstr.actual) (:expected test_konstr.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_konstr)
 (modules test_konstr)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

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

(rule
 (targets test_tuple.expected.format)
 (deps (:pp pp.exe) (:input test_tuple.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_tuple.actual)
 (deps (:pp pp.exe) (:input test_tuple.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_tuple.actual) (:expected test_tuple.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_tuple)
 (modules test_tuple)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))
