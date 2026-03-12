(defpackage #:mallet/tests/rules/double-colon-access
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/double-colon-access)

(defun check-double-colon (code)
  "Check CODE for double-colon-access violations."
  (let ((tokens (parser:tokenize code #p"test.lisp"))
        (rule (make-instance 'rules:double-colon-access-rule)))
    (rules:check-tokens rule tokens #p"test.lisp")))

;;; Valid cases (no violations)

(deftest double-colon-access-valid
  (testing "Single colon is fine (public access)"
    (ok (null (check-double-colon "(foo:bar baz)"))))

  (testing "Unqualified symbols are fine"
    (ok (null (check-double-colon "(defun foo (x) (+ x 1))"))))

  (testing "Uninterned symbols #: are fine"
    (ok (null (check-double-colon "(defpackage #:my-package (:use #:cl))"))))

  (testing "Keywords are fine"
    (ok (null (check-double-colon "(foo :key :value)"))))

  (testing "String with double colon is not a violation"
    (ok (null (check-double-colon "(print \"foo::bar\")"))))

  (testing "Normal code with no package qualifiers is fine"
    (ok (null (check-double-colon "(defun test () (let ((x 1)) (+ x 2)))"))))

  (testing "Multiple single-colon qualified symbols are fine"
    (ok (null (check-double-colon "(foo:bar baz:qux)"))))

  (testing "Package keyword :keyword-name is fine"
    (ok (null (check-double-colon "(:use #:cl #:alexandria)")))))

;;; Invalid cases (violations expected)

(deftest double-colon-access-invalid
  (testing "Simple double-colon access is flagged"
    (let ((violations (check-double-colon "(foo::bar baz)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :double-colon-access))
      (ok (eq (violation:violation-severity (first violations)) :warning))))

  (testing "Double-colon in function call is flagged"
    (let ((violations (check-double-colon "(some-package::internal-function arg)")))
      (ok (= (length violations) 1))
      (ok (search "some-package::internal-function"
                  (violation:violation-message (first violations))))))

  (testing "Multiple double-colon accesses are all flagged"
    (let ((violations (check-double-colon "(foo::bar baz::qux)")))
      (ok (= (length violations) 2))))

  (testing "Double-colon in defun body is flagged"
    (let ((violations (check-double-colon "(defun test () (pkg::internal-fn 42))")))
      (ok (= (length violations) 1))))

  (testing "Double-colon with uppercase package is flagged"
    (let ((violations (check-double-colon "(FOO::BAR)")))
      (ok (= (length violations) 1))))

  (testing "Violation reports correct line number"
    (let ((violations (check-double-colon "(defun foo ()
  (pkg::bar 42))")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 2))))

  (testing "Violation reports correct column number"
    (let ((violations (check-double-colon "(foo::bar)")))
      (ok (= (violation:violation-column (first violations)) 1))))

  (testing "Double-colon in nested form is flagged"
    (let ((violations (check-double-colon "(let ((x (pkg::internal-value))) x)")))
      (ok (= (length violations) 1)))))

;;; Edge cases

(deftest double-colon-access-edge-cases
  (testing "Keyword symbol :foo is not a violation"
    (ok (null (check-double-colon ":foo"))))

  (testing "Uninterned reader macro symbol #:foo is not a violation"
    (ok (null (check-double-colon "#:foo"))))

  (testing "Empty input has no violations"
    (ok (null (check-double-colon ""))))

  (testing "Just a comment has no violations"
    (ok (null (check-double-colon ";; This is a comment with :: text"))))

  (testing "Escaped quote artifact (foo::bar backslash) is not flagged"
    ;; The tokenizer produces symbol tokens with trailing backslash from escaped
    ;; quotes inside string literals, e.g. \"PKG::NAME\" in a docstring.
    ;; These are not real double-colon accesses and must not be flagged.
    (let ((rule (make-instance 'rules:double-colon-access-rule))
          (tokens (parser:tokenize "(defun f () \"like \\\"foo::bar\\\" text\")" #p"test.lisp")))
      (ok (null (rules:check-tokens rule tokens #p"test.lisp"))))))

;;; Registration tests

(deftest double-colon-access-registration
  (testing ":double-colon-access is in default config"
    (let* ((cfg (mallet/config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (member :double-colon-access rule-names))))

  (testing ":double-colon-access is in :all config"
    (let* ((cfg (mallet/config:get-built-in-config :all))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (member :double-colon-access rule-names))))

  (testing ":double-colon-access rule has :practice category"
    (let ((rule (rules:make-rule :double-colon-access)))
      (ok (eq :practice (rules:rule-category rule))))))

;;; Test file skipping

(deftest test-file-skipping
  (testing "File with rove defpackage is skipped"
    (let* ((test-code "(defpackage #:my-tests
  (:use #:cl #:rove))
(in-package #:my-tests)
(deftest foo
  (ok (null pkg::internal)))")
           (violations (check-double-colon test-code)))
      (ok (null violations)
          "Test file with rove usage should have no violations")))

  (testing "File with fiveam defpackage is skipped"
    (let* ((test-code "(defpackage #:my-tests
  (:use #:cl #:fiveam))
(in-package #:my-tests)
(def-test foo ()
  (is (null pkg::internal)))")
           (violations (check-double-colon test-code)))
      (ok (null violations)
          "Test file with fiveam usage should have no violations")))

  (testing "File with import-from test framework is skipped"
    (let* ((test-code "(defpackage #:my-tests
  (:use #:cl)
  (:import-from #:rove #:ok #:deftest))
(in-package #:my-tests)
(deftest bar
  (ok (null pkg::internal)))")
           (violations (check-double-colon test-code)))
      (ok (null violations)
          "Test file importing from rove should have no violations")))

  (testing "Regular non-test file is not skipped"
    (let* ((normal-code "(defpackage #:my-pkg
  (:use #:cl))
(in-package #:my-pkg)
(defun foo ()
  other-pkg::internal-fn)")
           (violations (check-double-colon normal-code)))
      (ok (= 1 (length violations))
          "Non-test file should report violations")))

  (testing "Defpackage with :export #:rove does not trigger test-file skipping"
    (let* ((code "(defpackage #:foo (:export #:rove))
(in-package #:foo)
(defun bar () pkg::sym)")
           (violations (check-double-colon code)))
      (ok (= 1 (length violations))
          "Export clause with framework name should not suppress violations"))))

;;; :skip-test-files nil option

(deftest skip-test-files-nil
  (testing "With :skip-test-files nil, test files are still checked"
    (let* ((rule (make-instance 'rules:double-colon-access-rule
                                :skip-test-files nil))
           (test-code "(defpackage #:test-pkg (:use #:cl #:rove))
(in-package #:test-pkg)
(deftest foo
  (ok (null pkg::internal-sym)))")
           (tokens (parser:tokenize test-code #p"test.lisp"))
           (violations (rules:check-tokens rule tokens #p"test.lisp")))
      (ok (= 1 (length violations))
          "Violations should be reported when skip-test-files is nil"))))
