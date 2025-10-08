(defpackage #:malvolio/tests/rules/unused-variables
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malvolio/rules)
   (#:parser #:malvolio/parser)
   (#:violation #:malvolio/violation)))
(in-package #:malvolio/tests/rules/unused-variables)

(deftest unused-variables-valid
  (testing "Valid: all parameters used"
    (let* ((code "(defun add (x y)
                     (+ x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter with declare ignore"
    (let* ((code "(defun add (x y)
                     (declare (ignore y))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter with underscore prefix"
    (let* ((code "(defun add (x _y)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter named underscore"
    (let* ((code "(lambda (_ x)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: all let bindings used"
    (let* ((code "(let ((x 1) (y 2))
                     (+ x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: let binding with declare ignore"
    (let* ((code "(let ((x 1) (y 2))
                     (declare (ignore y))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest unused-variables-invalid
  (testing "Invalid: unused function parameter"
    (let* ((code "(defun add (x y)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :unused-variables))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: multiple unused parameters"
    (let* ((code "(defun foo (x y z)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v)
                   (eq (violation:violation-rule v) :unused-variables))
                 violations))))

  (testing "Invalid: unused lambda parameter"
    (let* ((code "(lambda (x y)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: unused let binding"
    (let* ((code "(let ((x 1) (y 2))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: unused let* binding"
    (let* ((code "(let* ((x 1) (y 2))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: unused loop variable"
    (let* ((code "(loop for item in list
                        for index from 0
                        collect item)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Loop variable 'index' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: nested unused variable"
    (let* ((code "(defun outer (x)
                     (let ((y 1))
                       (+ x 1)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations)))))))
