(defpackage #:mallet/tests/rules/needless-let-star
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/needless-let-star)

(deftest needless-let-star-valid
  (testing "Let* with sequential dependency is allowed"
    (let* ((code "(let* ((x 1)
                        (y (+ x 1)))
                     (list x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:needless-let*-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Self-shadowing binding counts as dependency"
    (let* ((code "(let* ((x 1)
                        (x (+ x 1)))
                     x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:needless-let*-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Empty binding list is ignored"
    (let* ((code "(let* ()
                     (do-something))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:needless-let*-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest needless-let-star-invalid
  (testing "Single binding let* should be reported"
    (let* ((code "(let* ((x 1))
                     x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:needless-let*-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :needless-let*))
      (ok (eq (violation:violation-severity (first violations)) :info))
      (ok (search "Use 'let' instead of 'let*'" (violation:violation-message (first violations))))))

  (testing "Independent let* bindings should be reported"
    (let* ((code "(let* ((x (foo))
                        (y (bar)))
                     (list x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:needless-let*-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :needless-let*))))

  (testing "Bindings that only read outer scope should be reported"
    (let* ((code "(let ((a 1))
                     (let* ((x (+ a 1))
                            (y (+ a 2)))
                       (list x y)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:needless-let*-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :needless-let*)))))
