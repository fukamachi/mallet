(defpackage #:malo/tests/rules/special-forms
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:malo/rules)
   (#:parser #:malo/parser)
   (#:violation #:malo/violation)))
(in-package #:malo/tests/rules/special-forms)

(deftest defstruct-no-false-positives
  (testing "DEFSTRUCT slots should not be treated as variable bindings"
    (let* ((code "(defstruct my-struct
                    (do #'identity :type function)
                    (variable #'list :type function))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFSTRUCT with 'do' and 'variable' slot names"))))

(deftest defclass-no-false-positives
  (testing "DEFCLASS slots should not be treated as variable bindings"
    (let* ((code "(defclass my-class ()
                    ((do :initarg :do :accessor get-do)
                     (variable :initarg :var)))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFCLASS with 'do' and 'variable' slot names"))))

(deftest define-condition-no-false-positives
  (testing "DEFINE-CONDITION slots should not be treated as variable bindings"
    (let* ((code "(define-condition my-condition (error)
                    ((do :initarg :do :reader condition-do)
                     (variable :initarg :var)))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFINE-CONDITION with 'do' and 'variable' slot names"))))

(deftest define-condition-report-checks-body
  (testing "DEFINE-CONDITION :report lambda should be checked for unused variables"
    (let* ((code "(define-condition my-error (error)
                    ((slot1 :initarg :slot1))
                    (:report (lambda (condition stream)
                               (let ((unused-in-report 3))
                                 (format stream \"Error: ~A\" (slot1 condition))))))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused variable in DEFINE-CONDITION :report")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'unused-in-report' is unused")
          "Correct violation message"))))

(deftest defpackage-no-false-positives
  (testing "DEFPACKAGE clauses should not be treated as code"
    (let* ((code "(defpackage #:test-pkg
                    (:use #:cl)
                    (:export #:do #:variable))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFPACKAGE with clauses containing 'do' and 'variable'"))))

(deftest deftype-no-false-positives
  (testing "DEFTYPE lambda list should not be checked for unused variables"
    (let* ((code "(deftype my-type (do variable)
                    `(or ,do ,variable))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFTYPE with type parameters named 'do' and 'variable'"))))

(deftest defgeneric-no-false-positives
  (testing "DEFGENERIC parameters should not be checked for unused variables"
    (let* ((code "(defgeneric compute (do variable)
                    (:documentation \"Compute something\"))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFGENERIC with parameters named 'do' and 'variable'"))))

(deftest defgeneric-method-checks-body
  (testing "DEFGENERIC :method body should be checked for unused variables"
    (let* ((code "(defgeneric foo (a b)
                    (:method ((a integer) b)
                      (let ((unused-var 1))
                        b)))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused variable in DEFGENERIC :method body")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'unused-var' is unused")
          "Correct violation message"))))

(deftest defmethod-no-false-positives
  (testing "DEFMETHOD parameters should not be checked for unused variables"
    (let* ((code "(defmethod compute ((do my-class) variable)
                    (print do)
                    (print variable))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFMETHOD with specialized parameter named 'do'"))))

(deftest defmethod-checks-body
  (testing "DEFMETHOD body should be checked for unused variables"
    (let* ((code "(defmethod compute ((a my-class) b)
                    (let ((unused-var 1))
                      (values a b)))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused variable in DEFMETHOD body")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'unused-var' is unused")
          "Correct violation message"))))

(deftest define-compiler-macro-no-false-positives
  (testing "DEFINE-COMPILER-MACRO parameters should not be checked"
    (let* ((code "(define-compiler-macro my-macro (do &optional variable)
                    `(progn ,do ,variable))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFINE-COMPILER-MACRO with parameter named 'do'"))))

(deftest define-compiler-macro-checks-body
  (testing "DEFINE-COMPILER-MACRO body should be checked for unused variables"
    (let* ((code "(define-compiler-macro my-macro (a &optional b)
                    (let ((unused-var 2))
                      `(+ ,a ,b)))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused variable in DEFINE-COMPILER-MACRO body")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'unused-var' is unused")
          "Correct violation message"))))

(deftest defsetf-no-false-positives
  (testing "DEFSETF should not be checked for unused variables"
    (let* ((code "(defsetf my-accessor (do variable) (new-value)
                    `(set-accessor ,do ,variable ,new-value))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFSETF with parameters named 'do' and 'variable'"))))

(deftest define-modify-macro-no-false-positives
  (testing "DEFINE-MODIFY-MACRO should not be checked for unused variables"
    (let* ((code "(define-modify-macro my-incf (do &optional (variable 1))
                    +)")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFINE-MODIFY-MACRO with parameter named 'do'"))))

(deftest define-setf-expander-no-false-positives
  (testing "DEFINE-SETF-EXPANDER should not be checked for unused variables"
    (let* ((code "(define-setf-expander my-place (do variable)
                    (values do variable nil nil nil))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violations for DEFINE-SETF-EXPANDER with parameters named 'do' and 'variable'"))))
