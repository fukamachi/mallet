(defpackage #:mallet/tests/rules/bare-progn
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/bare-progn)

(deftest bare-progn-valid
  (testing "Valid: if without progn"
    (let* ((code "(if condition
                     (do-something)
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: cond instead of if with progn"
    (let* ((code "(cond
                     (condition
                      (do-something)
                      (do-another))
                     (t (do-else)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
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
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: and without progn"
    (let* ((code "(and x y)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: or without progn"
    (let* ((code "(or x y)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: and with progn in non-last position"
    (let* ((code "(and (progn (a) (b)) x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: or with progn in non-last position"
    (let* ((code "(or (progn (a) (b)) x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest bare-progn-invalid
  (testing "Invalid: progn in then clause of if"
    (let* ((code "(if condition
                     (progn
                       (do-something)
                       (do-another))
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'cond' instead of 'if' with bare 'progn'"))))

  (testing "Invalid: progn in else clause of if"
    (let* ((code "(if condition
                     (do-something)
                     (progn
                       (do-else-1)
                       (do-else-2)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn))))

  (testing "Invalid: progn in both clauses of if"
    (let* ((code "(if condition
                     (progn
                       (do-something)
                       (do-another))
                     (progn
                       (do-else-1)
                       (do-else-2)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should only report one violation per IF form, even if both clauses have progn
      (ok (= (length violations) 1))))

  (testing "Invalid: and with progn as last argument"
    (let* ((code "(and x (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'when' instead of 'and' with bare 'progn'"))))

  (testing "Invalid: or with progn as last argument"
    (let* ((code "(or x (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'unless' instead of 'or' with bare 'progn'"))))

  (testing "Invalid: and with multiple args, last is progn"
    (let* ((code "(and x y (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'when' instead of 'and' with bare 'progn'"))))

  (testing "Invalid: or with multiple args, last is progn"
    (let* ((code "(or x y (progn (a) (b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'unless' instead of 'or' with bare 'progn'")))))
