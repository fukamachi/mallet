(defpackage #:mallet/tests/rules/wrong-otherwise
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/wrong-otherwise)

(deftest wrong-otherwise-valid
  (testing "Valid: ecase without otherwise"
    (let* ((code "(ecase value
                     (:a 1)
                     (:b 2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: etypecase without otherwise"
    (let* ((code "(etypecase value
                     (integer 1)
                     (string 2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: case with otherwise (should be allowed)"
    (let* ((code "(case value
                     (:a 1)
                     (:b 2)
                     (otherwise 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: typecase with otherwise (should be allowed)"
    (let* ((code "(typecase value
                     (integer 1)
                     (string 2)
                     (otherwise 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest wrong-otherwise-invalid
  (testing "Invalid: ecase with otherwise"
    (let* ((code "(ecase value
                     (:a 1)
                     (:b 2)
                     (otherwise 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :wrong-otherwise))
      (ok (string= (violation:violation-message (first violations))
                   "'ecase' should not have 'otherwise' or 't' clause"))))

  (testing "Invalid: ecase with t"
    (let* ((code "(ecase value
                     (:a 1)
                     (:b 2)
                     (t 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :wrong-otherwise))))

  (testing "Invalid: etypecase with otherwise"
    (let* ((code "(etypecase value
                     (integer 1)
                     (string 2)
                     (otherwise 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :wrong-otherwise))
      (ok (string= (violation:violation-message (first violations))
                   "'etypecase' should not have 'otherwise' or 't' clause"))))

  (testing "Invalid: etypecase with t"
    (let* ((code "(etypecase value
                     (integer 1)
                     (string 2)
                     (t 0))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:wrong-otherwise-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :wrong-otherwise)))))
