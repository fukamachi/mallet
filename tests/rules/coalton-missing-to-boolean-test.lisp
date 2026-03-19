(defpackage #:mallet/tests/rules/coalton-missing-to-boolean
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)
   (#:config #:mallet/config)))
(in-package #:mallet/tests/rules/coalton-missing-to-boolean)

;;; Helper

(defun check-code (code)
  "Run the coalton-missing-to-boolean rule on CODE and return all violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:coalton-missing-to-boolean-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases — no violations expected

(deftest coalton-missing-to-boolean-valid
  (testing "Valid: (lisp Boolean ...) with to-boolean"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Boolean (x) (to-boolean (null x)))))"))))

  (testing "Valid: (lisp Boolean ...) with qualified coalton:to-boolean"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Boolean (x) (coalton:to-boolean (null x)))))"))))

  (testing "Valid: (lisp Boolean ...) with coalton-prelude:to-boolean"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Boolean (x) (coalton-prelude:to-boolean (null x)))))"))))

  (testing "Valid: (lisp Integer ...) — non-Boolean type, no rule applies"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Integer (x) (+ x 1))))"))))

  (testing "Valid: (lisp String ...) — non-Boolean type, no rule applies"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp String (x) (format nil \"~A\" x))))"))))

  (testing "Valid: to-boolean inside nested call in body"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Boolean (x) (progn (to-boolean (member x '(1 2 3)))))))"))))

  (testing "Valid: empty coalton-toplevel — no lisp forms"
    (ok (null (check-code "(coalton-toplevel)"))))

  (testing "Valid: coalton-toplevel without any lisp form"
    (ok (null (check-code
               "(coalton-toplevel
                  (define x 42)
                  (define y \"hello\"))"))))

  (testing "Valid: regular CL defun — rule does not fire outside coalton-toplevel"
    (ok (null (check-code
               "(defun foo (x)
                  (lisp Boolean (x) (null x)))"))))

  (testing "Valid: (lisp Boolean ...) with to-boolean in nested let binding"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Boolean (x)
                      (let ((result (to-boolean (null x))))
                        result))))"))))

  (testing "Valid: (lisp (Optional Boolean) ...) — composite type, not plain Boolean"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp (Optional Boolean) (x) (null x))))"))))

  (testing "Valid: (lisp (Result Boolean String) ...) — parameterized type, not plain Boolean"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp (Result Boolean String) (x) (null x))))"))))

  (testing "Valid: (lisp Boolean () True) — Coalton Boolean literal True"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (always-true)
                    (lisp Boolean () True)))"))))

  (testing "Valid: (lisp Boolean () False) — Coalton Boolean literal False"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (always-false)
                    (lisp Boolean () False)))"))))

  (testing "Valid: to-boolean nested deeply in body via multiple levels"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Boolean (x)
                      (let ((result (progn (to-boolean (cl:evenp x)))))
                        result))))"))))

  (testing "Valid: empty body (lisp Boolean ()) — degenerate form, no CL predicate to wrap"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo)
                    (lisp Boolean ())))"))))

  (testing "Valid: lisp Boolean inside Coalton let — to-boolean present"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (let ((result (lisp Boolean (x) (to-boolean (null x)))))
                      result)))"))))

  (testing "Valid: lisp form with Unit type — not Boolean"
    (ok (null (check-code
               "(coalton-toplevel
                  (define (foo x)
                    (lisp Unit (x) (cl:print x))))")))))

;;; Invalid cases — violations expected

(deftest coalton-missing-to-boolean-invalid
  (testing "Invalid: (lisp Boolean ...) body returns raw Lisp predicate result"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (lisp Boolean (x) (null x))))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :coalton-missing-to-boolean))))

  (testing "Invalid: (lisp Boolean ...) body returns Lisp T directly"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (lisp Boolean () t)))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :coalton-missing-to-boolean))))

  (testing "Invalid: (lisp Boolean ...) body returns Lisp NIL directly"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (lisp Boolean () nil)))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :coalton-missing-to-boolean))))

  (testing "Invalid: case-insensitive — lowercase boolean triggers"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (lisp boolean (x) (null x))))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :coalton-missing-to-boolean))))

  (testing "Invalid: uppercase BOOLEAN triggers"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (lisp BOOLEAN (x) (null x))))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :coalton-missing-to-boolean))))

  (testing "Invalid: multiple lisp Boolean forms — both flagged"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (is-zero x)
                            (lisp Boolean (x) (zerop x)))
                          (define (is-nil x)
                            (lisp Boolean (x) (null x))))")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v) (eq (violation:violation-rule v) :coalton-missing-to-boolean))
                 violations))))

  (testing "Invalid: one lisp Boolean with to-boolean, one without — only one flagged"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (is-zero x)
                            (lisp Boolean (x) (to-boolean (zerop x))))
                          (define (is-nil x)
                            (lisp Boolean (x) (null x))))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :coalton-missing-to-boolean))))

  (testing "Invalid: lisp Boolean inside Coalton let — missing to-boolean"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (let ((result (lisp Boolean (x) (null x))))
                              result)))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :coalton-missing-to-boolean))))

  (testing "Invalid: lisp Boolean inside Coalton match — missing to-boolean"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (match x
                              ((Some y) (lisp Boolean (y) (null y)))
                              ((None) (lisp Boolean () nil)))))")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v) (eq (violation:violation-rule v) :coalton-missing-to-boolean))
                 violations))))

  (testing "Invalid: multiple lisp Boolean in same define, mixed with and without to-boolean"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x y)
                            (if (lisp Boolean (x) (to-boolean (null x)))
                                (lisp Boolean (y) (zerop y))
                                (lisp Boolean (x) (cl:evenp x)))))")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v) (eq (violation:violation-rule v) :coalton-missing-to-boolean))
                 violations))))

  (testing "Invalid: lisp Boolean with raw CL values T/NIL"
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (always-true)
                            (lisp Boolean () t))
                          (define (always-false)
                            (lisp Boolean () nil)))")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v) (eq (violation:violation-rule v) :coalton-missing-to-boolean))
                 violations)))))

;;; Violation metadata

(deftest coalton-missing-to-boolean-metadata
  (testing "Violation rule name is :coalton-missing-to-boolean"
    (let* ((code "(coalton-toplevel (define (foo x) (lisp Boolean (x) (null x))))")
           (violations (check-code code)))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :coalton-missing-to-boolean))))

  (testing "Violation message is non-empty string"
    (let* ((code "(coalton-toplevel (define (foo x) (lisp Boolean (x) (null x))))")
           (violations (check-code code)))
      (ok (stringp (violation:violation-message (first violations))))
      (ok (> (length (violation:violation-message (first violations))) 0))))

  (testing "Violation severity is :warning"
    (let* ((code "(coalton-toplevel (define (foo x) (lisp Boolean (x) (null x))))")
           (violations (check-code code)))
      (ok (eq (violation:violation-severity (first violations)) :warning))))

  (testing "Violation file matches"
    (let* ((code "(coalton-toplevel (define (foo x) (lisp Boolean (x) (null x))))")
           (violations (check-code code)))
      (ok (pathnamep (violation:violation-file (first violations))))))

  (testing "Violation line is a positive integer"
    (let* ((code "(coalton-toplevel (define (foo x) (lisp Boolean (x) (null x))))")
           (violations (check-code code)))
      (ok (and (integerp (violation:violation-line (first violations)))
               (>= (violation:violation-line (first violations)) 1))))))

;;; Stub guard — ensures a no-op implementation would NOT pass

(deftest coalton-missing-to-boolean-stub-guard
  (testing "A stub returning empty violations must fail on known-bad input"
    ;; This test guards against a degenerate implementation that always returns nil.
    ;; If check-code returns violations for known-bad input, the rule is real.
    (let ((violations (check-code
                       "(coalton-toplevel
                          (define (foo x)
                            (lisp Boolean (x) (null x))))")))
      (ok (not (null violations))
          "Rule must detect missing to-boolean — a stub returning nil is broken"))))

;;; Rule class metadata

(deftest coalton-missing-to-boolean-rule-metadata
  (testing "Rule is a coalton-rule subclass"
    (ok (subtypep 'rules:coalton-missing-to-boolean-rule 'base:coalton-rule)))

  (testing "Rule name is :coalton-missing-to-boolean"
    (let ((rule (make-instance 'rules:coalton-missing-to-boolean-rule)))
      (ok (eq (base:rule-name rule) :coalton-missing-to-boolean))))

  (testing "Rule severity is :warning"
    (let ((rule (make-instance 'rules:coalton-missing-to-boolean-rule)))
      (ok (eq (base:rule-severity rule) :warning))))

  (testing "Rule category is :correctness"
    (let ((rule (make-instance 'rules:coalton-missing-to-boolean-rule)))
      (ok (eq (base:rule-category rule) :correctness))))

  (testing "Rule type is :form"
    (let ((rule (make-instance 'rules:coalton-missing-to-boolean-rule)))
      (ok (eq (base:rule-type rule) :form))))

  (testing "Rule is enabled by default"
    (let ((rule (make-instance 'rules:coalton-missing-to-boolean-rule)))
      (ok (base:rule-enabled-p rule))))

  (testing "make-rule dispatches :coalton-missing-to-boolean correctly"
    (let ((rule (rules:make-rule :coalton-missing-to-boolean)))
      (ok (typep rule 'rules:coalton-missing-to-boolean-rule))))

  (testing "Rule is included in all-rules preset"
    (let* ((cfg (config:get-built-in-config :all))
           (rule-names (mapcar #'base:rule-name (config:config-rules cfg))))
      (ok (member :coalton-missing-to-boolean rule-names)))))
