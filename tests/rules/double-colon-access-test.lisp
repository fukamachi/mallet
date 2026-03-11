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
  (let* ((tokens (parser:tokenize code #p"test.lisp"))
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
    (let* ((rule (make-instance 'rules:double-colon-access-rule))
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
