(defpackage #:mallet/tests/rules/redundant-progn
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/redundant-progn)

(deftest redundant-progn-valid
  (testing "Valid: progn with multiple body forms"
    (let* ((code "(progn (do-something) (do-another))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: empty progn"
    (let* ((code "(progn)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest redundant-progn-invalid
  (testing "Invalid: progn with exactly one body form"
    (let* ((code "(progn (do-something))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:redundant-progn-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :redundant-progn))
      (ok (string= (violation:violation-message (first violations))
                   "PROGN with a single body form is redundant")))))
