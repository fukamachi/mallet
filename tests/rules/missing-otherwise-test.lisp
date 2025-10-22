(defpackage #:mallet/tests/rules/missing-otherwise
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/missing-otherwise)

(deftest missing-otherwise-valid
  (testing "Valid: case with otherwise"
    (let* ((code "(case value
                     (:a 1)
                     (:b 2)
                     (otherwise 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: typecase with otherwise"
    (let* ((code "(typecase value
                     (integer 1)
                     (string 2)
                     (otherwise 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: ecase without otherwise (should use exhaustive)"
    (let* ((code "(ecase value
                     (:a 1)
                     (:b 2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: etypecase without otherwise (should use exhaustive)"
    (let* ((code "(etypecase value
                     (integer 1)
                     (string 2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest missing-otherwise-invalid
  (testing "Invalid: case without otherwise"
    (let* ((code "(case month
                     ((1 3 5 7 8 10 12) 31)
                     ((4 6 9 11) 30)
                     (2 28))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :missing-otherwise))
      (ok (string= (violation:violation-message (first violations))
                   "'case' should have 'otherwise' clause"))))

  (testing "Invalid: typecase without otherwise"
    (let* ((code "(typecase value
                     (integer (handle-int value))
                     (string (handle-str value)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :missing-otherwise))
      (ok (string= (violation:violation-message (first violations))
                   "'typecase' should have 'otherwise' clause"))))

  (testing "Invalid: case with t instead of otherwise"
    (let* ((code "(case value
                     (:a 1)
                     (:b 2)
                     (t 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'otherwise' instead of 't' in 'case'"))))

  (testing "Invalid: typecase with t instead of otherwise"
    (let* ((code "(typecase value
                     (integer 1)
                     (string 2)
                     (t 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:missing-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (string= (violation:violation-message (first violations))
                   "Use 'otherwise' instead of 't' in 'typecase'")))))
