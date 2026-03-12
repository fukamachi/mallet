(defpackage #:mallet/tests/rules/redundant-progn
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
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
  (testing "Violation has correct severity"
    (let* ((code "(progn (foo))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-severity (first violations)) :info))))

  (testing "Violation reports correct position"
    (let* ((code "(progn (foo))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 1))
      (ok (= (violation:violation-column (first violations)) 0)))))
