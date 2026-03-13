(defpackage #:mallet/tests/rules/error-with-string-only
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/error-with-string-only)

(defun check-error-string (code)
  "Check CODE for error-with-string-only violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:error-with-string-only-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases (no violations)

(deftest error-with-string-only-valid
  (testing "error with condition type symbol is fine"
    (ok (null (check-error-string "(error 'my-error)"))))

  (testing "error with condition type and initargs is fine"
    (ok (null (check-error-string "(error 'my-error :message \"bad\")"))))

  (testing "error with make-condition is fine"
    (ok (null (check-error-string "(error (make-condition 'my-error))"))))

  (testing "error with condition object variable is fine"
    (ok (null (check-error-string "(error condition)"))))

  (testing "unrelated function call is fine"
    (ok (null (check-error-string "(warn \"warning message\")"))))

  (testing "no calls at all is fine"
    (ok (null (check-error-string "(defun foo (x) (+ x 1))"))))

  (testing "quoted list with error is data, not a call"
    (ok (null (check-error-string "'(error \"message\")"))))

  (testing "error with no arguments is fine"
    (ok (null (check-error-string "(error)"))))

  (testing "signal with string is not flagged (different function)"
    (ok (null (check-error-string "(signal \"something\")"))))

  (testing "cerror with string is not flagged (different function)"
    (ok (null (check-error-string "(cerror \"continue\" \"something\")")))))

;;; Invalid cases (violations expected)

(deftest error-with-string-only-invalid
  (testing "Simple (error \"string\") is flagged"
    (let ((violations (check-error-string "(error \"something went wrong\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-with-string-only))
      (ok (eq (violation:violation-severity (first violations)) :warning))))

  (testing "Violation message mentions signaling a condition type"
    (let ((violations (check-error-string "(error \"bad\")")))
      (ok (= (length violations) 1))
      (ok (stringp (violation:violation-message (first violations))))))

  (testing "(error format-string arg) is flagged"
    (let ((violations (check-error-string "(error \"bad value ~A\" x)")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-with-string-only))))

  (testing "(error \"msg\") nested in defun is flagged"
    (let ((violations (check-error-string "(defun check (x)
  (when (null x)
    (error \"x must not be nil\")))")))
      (ok (= (length violations) 1))))

  (testing "Multiple error calls with strings are all flagged"
    (let ((violations (check-error-string "(progn
  (error \"first problem\")
  (error \"second problem\"))")))
      (ok (= (length violations) 2))))

  (testing "(cl:error \"msg\") qualified call is flagged"
    (let ((violations (check-error-string "(cl:error \"bad\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :error-with-string-only))))

  (testing "Violation reports correct line number"
    (let ((violations (check-error-string "(defun foo ()
  (error \"bad\"))")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 2))))

  (testing "error with string deeply nested is flagged"
    (let ((violations (check-error-string "(defun validate (x)
  (let ((y (1+ x)))
    (when (< y 0)
      (error \"y is negative\"))))")))
      (ok (= (length violations) 1)))))

;;; Edge cases

(deftest error-with-string-only-edge-cases
  (testing "error with empty string is flagged"
    (let ((violations (check-error-string "(error \"\")")))
      (ok (= (length violations) 1))))

  (testing "error symbol used as value (not a call) is fine"
    (ok (null (check-error-string "(let ((f #'error)) f)"))))

  (testing "error in a let binding position (function not called) is fine"
    (ok (null (check-error-string "(let ((error-fn #'error)) (funcall error-fn 'my-error))")))))

;;; Registration tests

(deftest error-with-string-only-registration
  (testing ":error-with-string-only is NOT in default config"
    (let* ((cfg (mallet/config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (not (member :error-with-string-only rule-names)))))

  (testing ":error-with-string-only IS in :all config"
    (let* ((cfg (mallet/config:get-built-in-config :all))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (member :error-with-string-only rule-names))))

  (testing ":error-with-string-only rule has :practice category"
    (let ((rule (rules:make-rule :error-with-string-only)))
      (ok (eq :practice (rules:rule-category rule))))))
