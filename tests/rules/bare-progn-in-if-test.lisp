(defpackage #:malvolio/tests/rules/bare-progn-in-if
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malvolio/rules)
   (#:parser #:malvolio/parser)
   (#:violation #:malvolio/violation)))
(in-package #:malvolio/tests/rules/bare-progn-in-if)

(deftest bare-progn-in-if-valid
  (testing "Valid: if without progn"
    (let* ((code "(if condition
                     (do-something)
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-in-if-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: cond instead of if with progn"
    (let* ((code "(cond
                     (condition
                      (do-something)
                      (do-another))
                     (t (do-else)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-in-if-rule))
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
           (rule (make-instance 'rules:bare-progn-in-if-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest bare-progn-in-if-invalid
  (testing "Invalid: progn in then clause"
    (let* ((code "(if condition
                     (progn
                       (do-something)
                       (do-another))
                     (do-else))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-in-if-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn-in-if))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'cond' instead of 'if' with bare 'progn'"))))

  (testing "Invalid: progn in else clause"
    (let* ((code "(if condition
                     (do-something)
                     (progn
                       (do-else-1)
                       (do-else-2)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-in-if-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :bare-progn-in-if))))

  (testing "Invalid: progn in both clauses"
    (let* ((code "(if condition
                     (progn
                       (do-something)
                       (do-another))
                     (progn
                       (do-else-1)
                       (do-else-2)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:bare-progn-in-if-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 2)))))
