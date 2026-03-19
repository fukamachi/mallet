(defpackage #:mallet/tests/rules/progn-in-conditional
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/progn-in-conditional)

(deftest progn-in-conditional-valid
  (testing "Valid: if without progn"
    (let* ((code "(if condition
                     (do-something)
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: cond instead of if with progn"
    (let* ((code "(cond
                     (condition
                      (do-something)
                      (do-another))
                     (t (do-else)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: progn nested inside other forms is OK"
    (let* ((code "(if condition
                     (let ((x 1))
                       (progn
                         (do-something)
                         (do-another)))
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: and without progn"
    (let* ((code "(and x y)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: or without progn"
    (let* ((code "(or x y)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: and with progn in non-last position"
    (let* ((code "(and (progn (a) (b)) x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: or with progn in non-last position"
    (let* ((code "(or (progn (a) (b)) x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest progn-in-conditional-invalid
  (testing "Invalid: progn in then clause of if"
    (let* ((code "(if condition
                     (progn
                       (do-something)
                       (do-another))
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :progn-in-conditional))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'cond' instead of 'if' with bare 'progn'"))))

  (testing "Invalid: progn in else clause of if"
    (let* ((code "(if condition
                     (do-something)
                     (progn
                       (do-else-1)
                       (do-else-2)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :progn-in-conditional))))

  (testing "Invalid: progn in both clauses of if"
    (let* ((code "(if condition
                     (progn
                       (do-something)
                       (do-another))
                     (progn
                       (do-else-1)
                       (do-else-2)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should only report one violation per IF form, even if both clauses have progn
      (ok (= (length violations) 1))))

  (testing "Invalid: and with progn as last argument"
    (let* ((code "(and x (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :progn-in-conditional))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'when' instead of 'and' with bare 'progn'"))))

  (testing "Invalid: or with progn as last argument"
    (let* ((code "(or x (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :progn-in-conditional))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'unless' instead of 'or' with bare 'progn'"))))

  (testing "Invalid: and with multiple args, last is progn"
    (let* ((code "(and x y (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :progn-in-conditional))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'when' instead of 'and' with bare 'progn'"))))

  (testing "Invalid: or with multiple args, last is progn"
    (let* ((code "(or x y (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :progn-in-conditional))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'unless' instead of 'or' with bare 'progn'")))))

;;; Coalton-aware tests

(deftest progn-in-conditional-coalton
  (testing "Valid: if without progn inside coalton-toplevel is not flagged"
    (let* ((code "(coalton-toplevel
                   (define (foo x)
                     (if x
                         (bar x)
                         (baz x))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Invalid: progn in if then-clause inside coalton-toplevel is flagged"
    (let* ((code "(coalton-toplevel
                   (define (foo x)
                     (if x
                         (progn
                           (bar x)
                           (baz x))
                         (qux x))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :progn-in-conditional))
      (ok (eq (violation:violation-severity (first violations)) :info))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'cond' instead of 'if' with bare 'progn'"))))

  (testing "Invalid: progn in if else-clause inside coalton-toplevel is flagged"
    (let* ((code "(coalton-toplevel
                   (define (foo x)
                     (if x
                         (bar x)
                         (progn
                           (baz x)
                           (qux x)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :progn-in-conditional))))

  (testing "Invalid: and with progn as last arg inside coalton-toplevel define"
    (let* ((code "(coalton-toplevel
                   (define (check x)
                     (and (valid? x) (progn (log x) (process x)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :progn-in-conditional))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'when' instead of 'and' with bare 'progn'"))))

  (testing "Invalid: or with progn as last arg inside coalton-toplevel define"
    (let* ((code "(coalton-toplevel
                   (define (fallback x)
                     (or (try-first x) (progn (log-failure x) (default-val)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :progn-in-conditional))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'unless' instead of 'or' with bare 'progn'")))))

(deftest progn-in-conditional-coalton-negative
  (testing "Negative: coalton-toplevel with normal if produces zero violations"
    (let* ((code "(coalton-toplevel
                   (define (decide x)
                     (if (> x 0)
                         (positive x)
                         (negative x))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Negative: coalton-toplevel with and/or but no progn produces zero violations"
    (let* ((code "(coalton-toplevel
                   (define (logic x y)
                     (and (check x) (check y))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest progn-in-conditional-coalton-regression
  (testing "Regression: CL if+progn outside coalton-toplevel still triggers violation"
    (let* ((code "(if condition
                     (progn (a) (b))
                     (c))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :progn-in-conditional))))

  (testing "Regression: CL and+progn outside coalton-toplevel still triggers violation"
    (let* ((code "(and x (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:progn-in-conditional-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :progn-in-conditional))))

  (testing "coalton-aware-p returns T for progn-in-conditional-rule"
    (let ((rule (make-instance 'rules:progn-in-conditional-rule)))
      (ok (rules:coalton-aware-p rule)))))
