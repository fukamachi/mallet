(defpackage #:mallet/tests/rules/redundant-progn
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:config #:mallet/config)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/redundant-progn)

;;; Valid cases — no violations expected

(deftest redundant-progn-valid
  (testing "Valid: progn with two body forms"
    (let* ((code "(progn (do-something) (do-another))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: progn with three body forms"
    (let* ((code "(progn (a) (b) (c))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: empty progn"
    (let* ((code "(progn)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: non-progn forms are not flagged"
    (let* ((code "(when condition (do-something))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: normal function call"
    (let* ((code "(foo bar baz)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: let form is not flagged"
    (let* ((code "(let ((x 1)) (+ x 2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: progn with ,@body splice in macro is not flagged"
    (let* ((code "(defmacro my-when (test &body body)
  `(when ,test (progn ,@body)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

;;; Invalid cases — violations expected

(deftest redundant-progn-invalid
  (testing "Invalid: progn with exactly one function-call body form"
    (let* ((code "(progn (do-something))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :redundant-progn))
      (ok (string= (violation:violation-message (first violations))
                   "PROGN with a single body form is redundant"))))

  (testing "Invalid: progn with single integer literal"
    (let* ((code "(progn 42)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Invalid: progn with single string literal"
    (let* ((code "(progn \"hello\")")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Invalid: progn with single symbol"
    (let* ((code "(progn x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Invalid: single-body progn inside defun body"
    (let* ((code "(defun foo () (progn (bar)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Invalid: single-body progn inside let body"
    (let* ((code "(let ((x 1)) (progn (use x)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Invalid: single-body progn inside when body"
    (let* ((code "(when condition (progn (do-it)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Invalid: nested single-body progns each flagged"
    (let* ((code "(progn (progn (x)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Both the outer (progn (progn (x))) and inner (progn (x)) should be flagged
      (ok (= (length violations) 2)))))

;;; Violation attribute tests

(deftest redundant-progn-attributes
  (testing "Rule has :warning default severity"
    (let ((rule (make-instance 'rules:redundant-progn-rule)))
      (ok (eq :warning (rules:rule-severity rule)))))

  (testing "Violation has correct severity"
    (let* ((code "(progn (foo))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-severity (first violations)) :warning))))

  (testing "Violation reports correct position"
    (let* ((code "(progn (foo))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 1))
      (ok (= (violation:violation-column (first violations)) 0)))))

;;; Registration tests — rule in presets and make-rule dispatch

(deftest redundant-progn-in-default-config
  (testing ":redundant-progn is NOT in :default config (moved to :strict)"
    (let* ((cfg (config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      (ok (not (member :redundant-progn rule-names)))))
  (testing ":redundant-progn IS in :strict config"
    (let* ((cfg (config:get-built-in-config :strict))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      (ok (member :redundant-progn rule-names)))))

(deftest redundant-progn-in-all-config
  (testing ":redundant-progn is in :all config"
    (let* ((cfg (config:get-built-in-config :all))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      (ok (member :redundant-progn rule-names)))))

(deftest redundant-progn-make-rule
  (testing "make-rule dispatches :redundant-progn"
    (let ((rule (rules:make-rule :redundant-progn)))
      (ok (typep rule 'rules:redundant-progn-rule))
      (ok (eq (rules:rule-name rule) :redundant-progn)))))

;;; Coalton-aware tests

(deftest redundant-progn-coalton
  (testing "Valid: progn with two body forms inside coalton-toplevel is not flagged"
    (let* ((code "(coalton-toplevel
                   (define (foo x)
                     (progn (bar x) (baz x))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: progn with multiple body forms inside coalton define is not flagged"
    (let* ((code "(coalton-toplevel
                   (define (process x)
                     (progn
                       (validate x)
                       (transform x)
                       (store x))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Invalid: single-body progn inside coalton-toplevel define is flagged"
    (let* ((code "(coalton-toplevel
                   (define (foo x)
                     (progn (bar x))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))
      (ok (eq (violation:violation-severity (first violations)) :warning))
      (ok (string= (violation:violation-message (first violations))
                   "PROGN with a single body form is redundant"))))

  (testing "Invalid: single-body progn nested in coalton-toplevel let is flagged"
    (let* ((code "(coalton-toplevel
                   (define (foo x)
                     (let ((y (progn (bar x))))
                       y)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Invalid: single-body progn inside Coalton match branch is flagged"
    (let* ((code "(coalton-toplevel
                   (define (handle x)
                     (match x
                       ((Some v) (progn (process v)))
                       ((None) (default-value)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn)))))

(deftest redundant-progn-coalton-negative
  (testing "Negative: coalton-toplevel with normal define produces zero violations"
    (let* ((code "(coalton-toplevel
                   (define (add x y)
                     (+ x y)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Negative: coalton-toplevel with if, let, match but no progn issues"
    (let* ((code "(coalton-toplevel
                   (define (complex-fn x)
                     (if (> x 0)
                         (let ((y (* x 2)))
                           y)
                         (match x
                           ((Some v) v)
                           ((None) 0)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest redundant-progn-coalton-regression
  (testing "Regression: CL progn outside coalton-toplevel still triggers violation"
    (let* ((code "(defun bar () (progn (baz)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "Regression: top-level progn outside coalton-toplevel still triggers"
    (let* ((code "(progn (only-one))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :redundant-progn))))

  (testing "coalton-aware-p returns T for redundant-progn-rule"
    (let ((rule (make-instance 'rules:redundant-progn-rule)))
      (ok (rules:coalton-aware-p rule)))))
