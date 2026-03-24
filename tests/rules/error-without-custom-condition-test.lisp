(defpackage #:mallet/tests/rules/error-without-custom-condition
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/error-without-custom-condition)

(defun check-error-custom (code)
  "Check CODE for error-without-custom-condition violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:error-without-custom-condition-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases (no violations)

(deftest error-without-custom-condition-valid
  (testing "error with custom condition type symbol is fine"
    (ok (null (check-error-custom "(error 'my-error)"))))

  (testing "error with custom condition type and initargs is fine"
    (ok (null (check-error-custom "(error 'my-error :message \"bad\")"))))

  (testing "error with make-condition is fine"
    (ok (null (check-error-custom "(error (make-condition 'my-error))"))))

  (testing "error with condition object variable is fine"
    (ok (null (check-error-custom "(error condition)"))))

  (testing "error with arbitrary variable is fine"
    (ok (null (check-error-custom "(error some-var)"))))

  (testing "error with my-custom-error is fine"
    (ok (null (check-error-custom "(error 'my-custom-error \"detail\")"))))

  (testing "unrelated function call is fine"
    (ok (null (check-error-custom "(warn \"warning message\")"))))

  (testing "no calls at all is fine"
    (ok (null (check-error-custom "(defun foo (x) (+ x 1))"))))

  (testing "quoted list with error is data, not a call"
    (ok (null (check-error-custom "'(error \"message\")"))))

  (testing "error with no arguments is fine"
    (ok (null (check-error-custom "(error)"))))

  (testing "signal with string is not flagged (different function)"
    (ok (null (check-error-custom "(signal \"something\")"))))

  (testing "cerror with string is not flagged (different function)"
    (ok (null (check-error-custom "(cerror \"continue\" \"something\")")))))

;;; Invalid cases (violations expected) — string literal first arg

(deftest error-without-custom-condition-invalid-string
  (testing "Simple (error \"string\") is flagged"
    (let ((violations (check-error-custom "(error \"something went wrong\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))
      (ok (eq (violation:violation-severity (first violations)) :warning))))

  (testing "Violation message is a string"
    (let ((violations (check-error-custom "(error \"bad\")")))
      (ok (= (length violations) 1))
      (ok (stringp (violation:violation-message (first violations))))))

  (testing "(error format-string arg) is flagged"
    (let ((violations (check-error-custom "(error \"bad value ~A\" x)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "(error \"msg\") nested in defun is flagged"
    (let ((violations (check-error-custom "(defun check (x)
  (when (null x)
    (error \"x must not be nil\")))")))
      (ok (= (length violations) 1))))

  (testing "Multiple error calls with strings are all flagged"
    (let ((violations (check-error-custom "(progn
  (error \"first problem\")
  (error \"second problem\"))")))
      (ok (= (length violations) 2))))

  (testing "(cl:error \"msg\") qualified call is flagged"
    (let ((violations (check-error-custom "(cl:error \"bad\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "Violation reports correct line number"
    (let ((violations (check-error-custom "(defun foo ()
  (error \"bad\"))")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 2))))

  (testing "error with string deeply nested is flagged"
    (let ((violations (check-error-custom "(defun validate (x)
  (let ((y (1+ x)))
    (when (< y 0)
      (error \"y is negative\"))))")))
      (ok (= (length violations) 1))))

  (testing "error with empty string is flagged"
    (let ((violations (check-error-custom "(error \"\")")))
      (ok (= (length violations) 1)))))

;;; Invalid cases (violations expected) — quoted CL-package condition symbols

(deftest error-without-custom-condition-invalid-cl-symbols
  (testing "(error 'cl:simple-error) is flagged"
    (let ((violations (check-error-custom "(error 'cl:simple-error \"bad\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "(error 'cl:type-error) is flagged"
    (let ((violations (check-error-custom "(error 'cl:type-error :datum x :expected-type 'integer)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "(error 'common-lisp:program-error) is flagged"
    (let ((violations (check-error-custom "(error 'common-lisp:program-error \"bad call\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition)))))

;;; Invalid cases (violations expected) — quoted unqualified known CL condition names

(deftest error-without-custom-condition-invalid-unqualified
  (testing "(error 'simple-error) is flagged"
    (let ((violations (check-error-custom "(error 'simple-error \"bad\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "(error 'error) is flagged"
    (let ((violations (check-error-custom "(error 'error \"something\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "(error 'condition) is flagged"
    (let ((violations (check-error-custom "(error 'condition \"base condition\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "(error 'type-error) unqualified is flagged"
    (let ((violations (check-error-custom "(error 'type-error :datum x :expected-type 'string)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition))))

  (testing "(error 'program-error) unqualified is flagged"
    (let ((violations (check-error-custom "(error 'program-error \"bad\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-without-custom-condition)))))

;;; Edge cases

(deftest error-without-custom-condition-edge-cases
  (testing "error symbol used as value (not a call) is fine"
    (ok (null (check-error-custom "(let ((f #'error)) f)"))))

  (testing "error in a let binding position (function not called) is fine"
    (ok (null (check-error-custom "(let ((error-fn #'error)) (funcall error-fn 'my-error))")))))

;;; Registration tests

(deftest error-without-custom-condition-registration
  (testing ":error-without-custom-condition is NOT in default config"
    (let* ((cfg (mallet/config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (not (member :error-without-custom-condition rule-names)))))

  (testing ":error-without-custom-condition IS in :all config"
    (let* ((cfg (mallet/config:get-built-in-config :all))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (member :error-without-custom-condition rule-names))))

  (testing ":error-without-custom-condition rule has :practice category"
    (let ((rule (rules:make-rule :error-without-custom-condition)))
      (ok (eq :practice (rules:rule-category rule))))))
