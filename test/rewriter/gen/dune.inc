
(rule
 (targets test_alpha.expected.format)
 (deps (:pp pp.exe) (:input test_alpha.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_alpha.actual)
 (deps (:pp pp.exe) (:input test_alpha.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_alpha.actual) (:expected test_alpha.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_alpha)
 (modules test_alpha)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_dependencies.expected.format)
 (deps (:pp pp.exe) (:input test_dependencies.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_dependencies.actual)
 (deps (:pp pp.exe) (:input test_dependencies.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_dependencies.actual) (:expected test_dependencies.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_dependencies)
 (modules test_dependencies)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_gadt.expected.format)
 (deps (:pp pp.exe) (:input test_gadt.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_gadt.actual)
 (deps (:pp pp.exe) (:input test_gadt.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_gadt.actual) (:expected test_gadt.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_gadt)
 (modules test_gadt)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

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
 (targets test_list.expected.format)
 (deps (:pp pp.exe) (:input test_list.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_list.actual)
 (deps (:pp pp.exe) (:input test_list.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_list.actual) (:expected test_list.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_list)
 (modules test_list)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_option.expected.format)
 (deps (:pp pp.exe) (:input test_option.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_option.actual)
 (deps (:pp pp.exe) (:input test_option.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_option.actual) (:expected test_option.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_option)
 (modules test_option)
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
 (targets test_record.expected.format)
 (deps (:pp pp.exe) (:input test_record.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_record.actual)
 (deps (:pp pp.exe) (:input test_record.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_record.actual) (:expected test_record.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_record)
 (modules test_record)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_recursive.expected.format)
 (deps (:pp pp.exe) (:input test_recursive.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_recursive.actual)
 (deps (:pp pp.exe) (:input test_recursive.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_recursive.actual) (:expected test_recursive.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_recursive)
 (modules test_recursive)
 (libraries zarith data-encoding)
 (preprocess (pps ppx_pbt)))

(rule
 (targets test_tree.expected.format)
 (deps (:pp pp.exe) (:input test_tree.expected.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (targets test_tree.actual)
 (deps (:pp pp.exe) (:input test_tree.ml))
 (action (bash "./%{pp} --impl %{input} -o %{targets}")))

(rule
 (alias runtest)
 (deps (:actual test_tree.actual) (:expected test_tree.expected.format))
 (action (diff %{expected} %{actual})))

(test
 (name test_tree)
 (modules test_tree)
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
